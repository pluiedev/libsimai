const std = @import("std");
const defs = @import("defs.zig");

const Reader = std.io.Reader;

pub const Error = DurationError || error{
    InvalidCharacter,
    ExpectedSetBpmEnd,
    ExpectedSetUnitEnd,
    UnexpectedDurationStart,
    IncompatibleNoteTypes,
    UnexpectedFlag,
};

pub const DurationError = error{
    ExpectedDurationStart,
    ExpectedDurationEnd,
    ExpectedUnit,
    ExpectedUnitColon,
    ExpectedMeasures,
    ExpectedSeconds,
};

pub fn Parser(comptime T: type) type {
    return struct {
        reader: T,
        allocator: std.mem.Allocator,

        stopped: bool = false,

        pub fn init(reader: T, allocator: std.mem.Allocator) @This() {
            return .{ .reader = reader, .allocator = allocator };
        }

        pub fn parse(self: *@This()) !?defs.Event {
            if (self.stopped) return null;

            while (true) {
                var ch = self.next() catch return error.EndOfStream;
                const c = &ch;

                switch (c.*) {
                    // Simai ignores whitespace.
                    ' ', '\t', '\r', '\n' => continue,

                    '(' => return .{ .set_bpm = try self.parseDecimalToTerminator(')', error.ExpectedSetBpmEnd) },
                    '{' => return .{ .set_unit = try self.parseDecimalToTerminator('}', error.ExpectedSetUnitEnd) },
                    ',' => return .{ .note = .rest },

                    inline 'A'...'E' => |region| {
                        c.* = self.next() catch if (region == 'E') {
                            self.stopped = true;
                            return null;
                        } else return error.EndOfStream;

                        const area: defs.TouchArea = area: {
                            const area_ = switch (region) {
                                // C1 and C2 are accepted, but they are just treated like C. Why??
                                'C' => switch (c.*) {
                                    // have to use @as here because otherwise comptime weirdness happens
                                    '1' => defs.TouchArea{ .c = @as(u1, 0) },
                                    '2' => defs.TouchArea{ .c = @as(u1, 1) },
                                    else => break :area .{ .c = null }, // keep c
                                },
                                else => if (posFromChar(c.*)) |pos|
                                    @unionInit(defs.TouchArea, &.{std.ascii.toLower(region)}, pos)
                                else
                                    return error.InvalidCharacter,
                            };
                            c.* = try self.next();
                            break :area area_;
                        };

                        return .{
                            .note = try self.parseNote(c.*, .{
                                .type = .{ .touch = .{ .area = area } },
                            }),
                        };
                    },

                    else => if (posFromChar(c.*)) |pos| {
                        const n = self.next() catch null;
                        return .{ .note = try self.parseNote(n, .{
                            .type = .{ .tap = .{ .pos = pos } },
                        }) };
                    } else return error.InvalidCharacter,
                }
            }
        }

        fn next(self: *@This()) !u8 {
            return self.reader.reader().readByte();
        }

        fn parseNote(self: *@This(), c_: ?u8, state_: ParseNoteState) !defs.Note {
            var state, var ch: ?u8 = .{ state_, c_ };

            while (ch) |*c| {
                switch (c.*) {
                    ',' => break,
                    'h' => {
                        state.type = switch (state.type) {
                            .tap => |tap| .{ .hold = .{ .pos = tap.pos } },
                            .touch => |touch| .{ .touch_hold = .{ .area = touch.area } },
                            .hold, .slide, .touch_hold => return error.IncompatibleNoteTypes,
                        };
                        try state.validateFlagCombination();
                    },
                    'b' => {
                        switch (state.type) {
                            .tap => {},
                            // Flags for holds must occur before the duration.
                            .hold => |v| if (v.duration) |_| return error.UnexpectedFlag,
                            // Flags for slides must occur *after* the duration. Why, simai??
                            .slide => |v| if (v.time == null) return error.UnexpectedFlag,
                            .touch, .touch_hold => return error.UnexpectedFlag,
                        }
                        state.flags.is_break = true;
                    },
                    'f' => {
                        switch (state.type) {
                            .touch => {},
                            // Flags for holds must occur before the duration.
                            .touch_hold => |v| if (v.duration) |_| return error.UnexpectedFlag,
                            .tap, .hold, .slide => return error.UnexpectedFlag,
                        }
                        state.flags.is_fireworks = true;
                    },
                    'x' => {
                        switch (state.type) {
                            .tap => {},
                            // Flags for holds must occur before the duration.ser.zig:158:31: error: no field named 'is_star' in struct 'parser.ParseNoteState'
                            .hold => |v| if (v.duration) |_| return error.UnexpectedFlag,
                            .slide, .touch, .touch_hold => return error.UnexpectedFlag,
                        }
                        state.flags.is_ex = true;
                    },
                    '$' => {
                        switch (state.type) {
                            .tap => {},
                            .hold, .slide, .touch, .touch_hold => return error.UnexpectedFlag,
                        }
                        state.flags.is_star = true;

                        c.* = try self.next();
                        if (c.* == '$') {
                            state.flags.is_rotating_star = true;
                        }

                        continue;
                    },
                    '[' => {
                        switch (state.type) {
                            .tap, .touch => return error.UnexpectedDurationStart,
                            .slide => |*slide| slide.time = try self.parseSlideTime(),
                            inline .hold, .touch_hold => |*hold| hold.duration = try self.parseDuration(),
                        }
                    },
                    // TODO: parse multiple slides
                    else => {
                        var segments = std.ArrayList(defs.Slide.Segment).init(self.allocator);

                        const pos = switch (state.type) {
                            .tap => |tap| tap.pos,
                            .touch, .hold, .slide, .touch_hold => return error.IncompatibleNoteTypes,
                        };
                        var current_start = pos;

                        while (true) {
                            const shape = try self.parseSlideShape(c);
                            const dest_pos = posFromChar(c.*) orelse return error.ExpectedSlideEndPos;

                            try shape.validate(current_start, dest_pos);
                            try segments.append(.{ .shape = shape, .dest_pos = dest_pos });

                            current_start = dest_pos;

                            c.* = try self.next();
                            if (c.* == '[') break;
                        }

                        state.type = .{ .slide = .{
                            .pos = pos,
                            .segments = segments,
                        } };
                        try state.validateFlagCombination();
                        continue;
                    },
                }
                ch = self.next() catch null;
            }

            return switch (state.type) {
                .tap => |tap| .{ .tap = .{ .pos = tap.pos, .flags = .{
                    .is_break = state.flags.is_break,
                    .is_ex = state.flags.is_ex,
                    .is_star = state.flags.is_star,
                    .is_rotating_star = state.flags.is_rotating_star,
                } } },
                .hold => |hold| .{ .hold = .{ .duration = hold.duration orelse return error.ExpectedDuration, .pos = hold.pos, .flags = .{
                    .is_break = state.flags.is_break,
                    .is_ex = state.flags.is_ex,
                } } },
                .slide => |slide| {
                    const time = slide.time orelse return error.ExpectedDuration;
                    return .{ .slide = .{ .delay = time.delay, .duration = time.duration, .segments = slide.segments, .start_pos = slide.pos, .flags = .{
                        .is_break = state.flags.is_break,
                    } } };
                },
                .touch => |touch| .{ .touch = .{
                    .area = touch.area,
                    .flags = .{ .is_fireworks = state.flags.is_fireworks },
                } },
                .touch_hold => |touch_hold| .{ .touch_hold = .{
                    .area = touch_hold.area,
                    .duration = touch_hold.duration orelse return error.ExpectedDuration,
                    .flags = .{ .is_fireworks = state.flags.is_fireworks },
                } },
            };
        }

        fn parseDuration(self: *@This()) !defs.Duration {
            const time = try self.parseDurationInner(false);
            return time.duration;
        }

        fn parseSlideTime(self: *@This()) !ParseNoteState.SlideTime {
            return self.parseDurationInner(true);
        }

        fn parseDurationInner(self: *@This(), with_delay: bool) !ParseNoteState.SlideTime {
            // Yeah, this is kinda shit. Blame simai, not me
            // This is the *third* attempt at getting it right...

            var c: u8 = undefined;
            var time: ParseNoteState.SlideTime = .{
                .delay = .one_beat,
                .duration = undefined,
            };

            const num = try self.parseDecimal(&c);
            switch (c) {
                '#' => {
                    const num2 = try self.parseDecimal(&c);

                    if (with_delay and c == '#') {
                        const delay = num orelse return error.ExpectedDelay;
                        time.delay = .{ .seconds = delay };

                        if (num2) |_| return error.DuplicateBpmMarkers;

                        const num3 = try self.parseDecimal(&c);
                        switch (c) {
                            '#' => {
                                // delay##bpm#unit:length
                                const bpm = num3 orelse return error.ExpectedBpm;
                                const unit = try self.parseDecimal(&c) orelse return error.ExpectedUnit;
                                if (c != ':') return error.ExpectedLength;
                                const length = try self.parseDecimal(&c) orelse return error.ExpectedLength;

                                time.duration = .{ .measures = .{
                                    .bpm = bpm,
                                    .unit = unit,
                                    .length = length,
                                } };
                            },
                            ':' => {
                                // delay##unit:length
                                const unit = num3 orelse return error.ExpectedUnit;
                                const length = try self.parseDecimal(&c) orelse return error.ExpectedLength;

                                time.duration = .{ .measures = .{
                                    .bpm = null,
                                    .unit = unit,
                                    .length = length,
                                } };
                            },
                            ']' => {
                                // delay##seconds
                                const seconds = num3 orelse return error.ExpectedSeconds;
                                time.duration = .{ .seconds = seconds };
                            },
                            else => return error.InvalidCharacter,
                        }
                    } else {
                        switch (c) {
                            ':' => {
                                // bpm#unit:length
                                const bpm = num orelse return error.ExpectedBpm;
                                const unit = num2 orelse return error.ExpectedUnit;
                                const length = try self.parseDecimal(&c) orelse return error.ExpectedLength;

                                time.duration = .{ .measures = .{
                                    .bpm = bpm,
                                    .unit = unit,
                                    .length = length,
                                } };
                            },
                            ']' => {
                                // #seconds
                                if (num) |_| return error.ExpectedLength;
                                const seconds = num2 orelse return error.ExpectedSeconds;

                                time.duration = .{ .seconds = seconds };
                            },
                            else => return error.InvalidCharacter,
                        }
                    }
                },
                ':' => {
                    // unit:length
                    const unit = num orelse return error.ExpectedUnit;
                    const length = try self.parseDecimal(&c) orelse return error.ExpectedLength;

                    time.duration = .{ .measures = .{
                        .bpm = null,
                        .unit = unit,
                        .length = length,
                    } };
                },
                else => return error.InvalidCharacter,
            }

            return time;
        }

        fn parseSlideShape(self: *@This(), c: *u8) !defs.Slide.Shape {
            const shape: defs.Slide.Shape = switch (c.*) {
                '-' => .straight,
                '>' => .{ .arc = .anticlockwise },
                '<' => .{ .arc = .clockwise },
                '^' => .{ .arc = null },
                'v' => .v,
                'V' => grand_v: {
                    c.* = try self.next();
                    const pos = posFromChar(c.*) orelse return error.ExpectedSlideMiddlePos;
                    break :grand_v .{ .grand_v = pos };
                },
                'w' => .fan,
                inline 'p', 'q' => |prev_c| loop: {
                    const dir = switch (prev_c) {
                        'p' => .clockwise,
                        'q' => .anticlockwise,
                        else => comptime unreachable,
                    };
                    c.* = try self.next();

                    if (prev_c == c.*) {
                        break :loop @unionInit(defs.Slide.Shape, "grand_loop", dir);
                    } else {
                        return @unionInit(defs.Slide.Shape, "loop", dir);
                    }
                },
                else => return error.InvalidCharacter,
            };
            c.* = try self.next();
            return shape;
        }

        fn parseDecimalToTerminator(self: *@This(), terminator: u8, err: Error) !f32 {
            var c: u8 = undefined;
            const num = try self.parseDecimal(&c) orelse return err;

            return if (c != terminator) err else num;
        }

        fn parseDecimal(self: *@This(), c: *u8) !?f32 {
            var n: u32 = 0;
            var consumed_chars: usize = 0;

            while (true) : (consumed_chars += 1) {
                c.* = self.next() catch break;
                const d = std.fmt.charToDigit(c.*, 10) catch break;
                n = n * 10 + d;
            }

            var i: usize = 0;

            if (c.* == '.') {
                while (true) : ({
                    i += 1;
                    consumed_chars += 1;
                }) {
                    c.* = self.next() catch break;
                    const d = std.fmt.charToDigit(c.*, 10) catch break;
                    n = n * 10 + d;
                }
            }

            const num: ?f32 = if (consumed_chars > 0)
                @as(f32, @floatFromInt(n)) / std.math.pow(f32, 10.0, @floatFromInt(i))
            else
                null;

            return num;
        }
    };
}
fn posFromChar(c: u8) ?defs.Position {
    return if (c >= '1' and c <= '8') @truncate(c - '1') else null;
}

const ParseNoteState = struct {
    type: Type,
    flags: packed struct {
        is_break: bool = false,
        is_ex: bool = false,
        is_star: bool = false,
        is_rotating_star: bool = false,
        is_fireworks: bool = false,
    } = .{},

    const Type = union(enum) {
        tap: struct {
            pos: defs.Position,
        },
        hold: struct {
            pos: defs.Position,
            duration: ?defs.Duration = null,
        },
        slide: struct {
            pos: defs.Position,
            time: ?SlideTime = null,
            segments: std.ArrayList(defs.Slide.Segment),
        },
        touch: struct {
            area: defs.TouchArea,
        },
        touch_hold: struct {
            area: defs.TouchArea,
            duration: ?defs.Duration = null,
        },
    };

    const SlideTime = struct {
        delay: defs.Slide.Delay = .one_beat,
        duration: defs.Duration,
    };

    fn validateFlagCombination(self: *@This()) !void {
        // Carry over flags. If there are flags that can't exist for the new type, bail.
        const flags = self.flags;
        const flags_invalid = switch (self.type) {
            .tap => flags.is_fireworks,
            .hold => flags.is_rotating_star or flags.is_star or flags.is_fireworks,
            .slide => flags.is_ex or flags.is_rotating_star or flags.is_star or flags.is_fireworks,
            .touch, .touch_hold => flags.is_break or flags.is_ex or flags.is_rotating_star or flags.is_star,
        };
        if (flags_invalid) return error.InvalidFlagCombination;
    }
};

fn initTestParser(c: []const u8) Parser(std.io.FixedBufferStream([]const u8)) {
    const input = std.io.fixedBufferStream(c);
    return Parser(@TypeOf(input)).init(input, std.testing.allocator);
}

test "bpm parses properly" {
    var parser = initTestParser(
        \\(120)
        \\(420.69)
        \\{2}
        \\{.114514}
        \\E
    );

    try std.testing.expectEqual(120.0, (try parser.parse()).?.set_bpm);
    try std.testing.expectEqual(420.69, (try parser.parse()).?.set_bpm);
    try std.testing.expectEqual(2.0, (try parser.parse()).?.set_unit);
    try std.testing.expectEqual(0.114514, (try parser.parse()).?.set_unit);

    try std.testing.expect(try parser.parse() == null);
}

test "tap parses properly" {
    var parser = initTestParser(
        \\1,2,3,4,5,6,7,8,
        \\1bx$$,2x$$b,
        \\E
    );

    for (0..8) |i_| {
        const i: defs.Position = @truncate(i_);
        try std.testing.expectEqual(i, (try parser.parse()).?.note.tap.pos);
    }

    var flags = (try parser.parse()).?.note.tap.flags;
    try std.testing.expect(flags.is_break and flags.is_ex and flags.is_star and flags.is_rotating_star);

    flags = (try parser.parse()).?.note.tap.flags;
    try std.testing.expect(flags.is_break and flags.is_ex and flags.is_star and flags.is_rotating_star);

    try std.testing.expect(try parser.parse() == null);
}

test "hold parses properly" {
    var parser = initTestParser(
        \\3bhx[4:3],
        \\2hb[#6.9],
        \\1h[150#4:3],
        \\E
    );

    var hold: defs.Hold = undefined;

    hold = (try parser.parse()).?.note.hold;
    try std.testing.expectEqual(2, hold.pos);
    try std.testing.expect(hold.flags.is_break and hold.flags.is_ex);
    try std.testing.expectEqual(defs.Duration{ .measures = .{
        .bpm = null,
        .unit = 4.0,
        .length = 3.0,
    } }, hold.duration);

    hold = (try parser.parse()).?.note.hold;
    try std.testing.expectEqual(1, hold.pos);
    try std.testing.expect(hold.flags.is_break and !hold.flags.is_ex);
    try std.testing.expectEqual(6.9, hold.duration.seconds);

    hold = (try parser.parse()).?.note.hold;
    try std.testing.expectEqual(0, hold.pos);
    try std.testing.expect(!hold.flags.is_break and !hold.flags.is_ex);
    try std.testing.expectEqual(defs.Duration{ .measures = .{
        .bpm = 150.0,
        .unit = 4.0,
        .length = 3.0,
    } }, hold.duration);

    try std.testing.expect(try parser.parse() == null);
}

test "slide duration parses properly" {
    var parser = initTestParser(
        \\4-2[4:3],
        \\4-2[#6.9],
        \\4-2[150#4:3],
        \\4-2[1.2##4:3],
        \\4-2[1.2##6.9],
        \\4-2[1.2##150#4:3],
        \\E
    );

    const times = [_]ParseNoteState.SlideTime{
        .{
            .duration = .{ .measures = .{
                .bpm = null,
                .unit = 4.0,
                .length = 3.0,
            } },
        },
        .{
            .duration = .{ .seconds = 6.9 },
        },
        .{
            .duration = .{ .measures = .{
                .bpm = 150.0,
                .unit = 4.0,
                .length = 3.0,
            } },
        },
        .{
            .delay = .{ .seconds = 1.2 },
            .duration = .{ .measures = .{
                .bpm = null,
                .unit = 4.0,
                .length = 3.0,
            } },
        },
        .{
            .delay = .{ .seconds = 1.2 },
            .duration = .{ .seconds = 6.9 },
        },
        .{
            .delay = .{ .seconds = 1.2 },
            .duration = .{ .measures = .{
                .bpm = 150.0,
                .unit = 4.0,
                .length = 3.0,
            } },
        },
    };

    for (times) |time| {
        const slide = (try parser.parse()).?.note.slide;
        defer slide.segments.deinit();

        const segment = slide.segments.items[0];

        try std.testing.expectEqual(time.delay, slide.delay);
        try std.testing.expectEqual(time.duration, slide.duration);
        try std.testing.expectEqual(3, slide.start_pos);
        try std.testing.expectEqual(1, segment.dest_pos);
        try std.testing.expectEqual(.straight, segment.shape);
    }

    try std.testing.expect(try parser.parse() == null);
}

test "slide shapes parse properly" {
    var parser = initTestParser(
        \\6-1^6<1>6p1q6pp1qq6v1V36w2[4:3],E
    );

    const shapes = [_]defs.Slide.Shape{
        .straight,
        .{ .arc = null },
        .{ .arc = .clockwise },
        .{ .arc = .anticlockwise },
        .{ .loop = .clockwise },
        .{ .loop = .anticlockwise },
        .{ .grand_loop = .clockwise },
        .{ .grand_loop = .anticlockwise },
        .v,
        .{ .grand_v = 2 },
        .fan,
    };

    const slide = (try parser.parse()).?.note.slide;
    defer slide.segments.deinit();

    var start: defs.Position, var dest: defs.Position = .{ 5, 0 };

    try std.testing.expectEqual(start, slide.start_pos);
    try std.testing.expectEqual(.one_beat, slide.delay);
    try std.testing.expectEqual(defs.Duration{ .measures = .{
        .bpm = null,
        .unit = 4.0,
        .length = 3.0,
    } }, slide.duration);

    for (slide.segments.items, shapes) |segment, shape| {
        try std.testing.expectEqual(if (shape == .fan) 1 else dest, segment.dest_pos);
        try std.testing.expectEqual(shape, segment.shape);

        std.mem.swap(defs.Position, &start, &dest);
    }

    try std.testing.expect(try parser.parse() == null);
}

test "touches parse properly" {
    var parser = initTestParser(
        \\A1,
        \\B2,
        \\C,
        \\C2f,
        \\D3f,
        \\E4f,
        \\E
    );

    var touch: defs.Touch = undefined;

    touch = (try parser.parse()).?.note.touch;
    try std.testing.expectEqual(defs.TouchArea{ .a = 0 }, touch.area);
    try std.testing.expect(!touch.flags.is_fireworks);

    touch = (try parser.parse()).?.note.touch;
    try std.testing.expectEqual(defs.TouchArea{ .b = 1 }, touch.area);
    try std.testing.expect(!touch.flags.is_fireworks);

    touch = (try parser.parse()).?.note.touch;
    try std.testing.expectEqual(defs.TouchArea{ .c = null }, touch.area);
    try std.testing.expect(!touch.flags.is_fireworks);

    touch = (try parser.parse()).?.note.touch;
    try std.testing.expectEqual(defs.TouchArea{ .c = 1 }, touch.area);
    try std.testing.expect(touch.flags.is_fireworks);

    touch = (try parser.parse()).?.note.touch;
    try std.testing.expectEqual(defs.TouchArea{ .d = 2 }, touch.area);
    try std.testing.expect(touch.flags.is_fireworks);

    touch = (try parser.parse()).?.note.touch;
    try std.testing.expectEqual(defs.TouchArea{ .e = 3 }, touch.area);
    try std.testing.expect(touch.flags.is_fireworks);

    try std.testing.expect(try parser.parse() == null);
}

test "touch holds parse properly" {
    var parser = initTestParser(
        \\Ch[4:3],
        \\A1fh[#6.9],
        \\E2hf[150#4:3],
        \\E
    );

    var touch_hold: defs.TouchHold = undefined;

    touch_hold = (try parser.parse()).?.note.touch_hold;
    try std.testing.expectEqual(defs.TouchArea{ .c = null }, touch_hold.area);
    try std.testing.expect(!touch_hold.flags.is_fireworks);
    try std.testing.expectEqual(defs.Duration{ .measures = .{
        .bpm = null,
        .unit = 4.0,
        .length = 3.0,
    } }, touch_hold.duration);

    touch_hold = (try parser.parse()).?.note.touch_hold;
    try std.testing.expectEqual(defs.TouchArea{ .a = 0 }, touch_hold.area);
    try std.testing.expect(touch_hold.flags.is_fireworks);
    try std.testing.expectEqual(defs.Duration{ .seconds = 6.9 }, touch_hold.duration);

    touch_hold = (try parser.parse()).?.note.touch_hold;
    try std.testing.expectEqual(defs.TouchArea{ .e = 1 }, touch_hold.area);
    try std.testing.expect(touch_hold.flags.is_fireworks);
    try std.testing.expectEqual(defs.Duration{ .measures = .{
        .bpm = 150.0,
        .unit = 4.0,
        .length = 3.0,
    } }, touch_hold.duration);

    try std.testing.expect(try parser.parse() == null);
}
