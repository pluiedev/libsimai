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
        c: ?u8 = null,
        stopped: bool = false,

        pub fn init(reader: T, allocator: std.mem.Allocator) @This() {
            return .{ .reader = reader, .allocator = allocator };
        }

        fn next(self: *@This()) !u8 {
            return self.reader.reader().readByte();
        }

        pub fn parse(self: *@This()) !?defs.Event {
            if (self.stopped) return null;
            const c = if (self.c) |c_| c: {
                self.c = null;
                break :c c_;
            } else self.next() catch return null;

            sw: switch (c) {
                // Simai ignores whitespace.
                ' ', '\t', '\r', '\n' => continue :sw self.next() catch return null,

                '(' => return .{ .set_bpm = try self.parseDecimalToTerminator(')', error.ExpectedSetBpmEnd) },
                '{' => return .{ .set_unit = try self.parseDecimalToTerminator('}', error.ExpectedSetUnitEnd) },
                ',' => return .{ .note = .rest },

                inline 'A'...'E' => |region| {
                    const n = self.next() catch if (region == 'E') {
                        self.stopped = true;
                        return null;
                    } else return error.EndOfStream;

                    const area: defs.TouchArea = switch (region) {
                        // C1 and C2 are accepted, but they are just treated like C. Why??
                        'C' => switch (n) {
                            // have to use @as here because otherwise comptime weirdness happens
                            '1' => .{ .c = @as(u1, 0) },
                            '2' => .{ .c = @as(u1, 1) },
                            else => keep_c: {
                                self.c = n;
                                break :keep_c .{ .c = null };
                            },
                        },
                        else => if (posFromChar(n)) |pos|
                            @unionInit(defs.TouchArea, &.{std.ascii.toLower(region)}, pos)
                        else
                            return error.InvalidCharacter,
                    };

                    return .{ .note = try self.parseNote(.{
                        .touch = .{ .area = area },
                    }) };
                },

                else => |char| {
                    return if (posFromChar(char)) |pos|
                        .{ .note = try self.parseNote(.{
                            .tap = .{ .pos = pos },
                        }) }
                    else
                        error.InvalidCharacter;
                },
            }
        }

        fn parseNote(self: *@This(), state_: ParseNoteState) !defs.Note {
            var state = state_;
            const c = if (self.c) |c_| c: {
                self.c = null;
                break :c c_;
            } else self.next() catch return state.finalize();

            sw: switch (c) {
                ',' => {},

                'h' => {
                    state = switch (state) {
                        .tap => |tap| .{ .hold = .{
                            .pos = tap.pos,
                            .flags = try translateFlags(tap.flags, defs.Hold.Flags),
                        } },
                        .touch => |touch| .{ .touch_hold = .{
                            .area = touch.area,
                            .flags = try translateFlags(touch.flags, defs.TouchHold.Flags),
                        } },
                        .hold, .slide, .touch_hold => return error.InvalidNoteType,
                    };
                    continue :sw self.next() catch break;
                },

                'b' => switch (state) {
                    inline .tap, .hold, .slide => |*v, tag| {
                        // Flags for holds must occur before the duration.
                        if (tag == .hold and v.duration != null) return error.InvalidFlagCombination;
                        // Flags for slides must occur *after* the duration. Why, simai??
                        if (tag == .slide and v.time == null) return error.InvalidFlagCombination;

                        v.flags.is_break = true;
                        continue :sw self.next() catch break;
                    },
                    // Flags for holds must occur before the duration.
                    .touch, .touch_hold => return error.InvalidFlagCombination,
                },

                'f' => switch (state) {
                    inline .touch, .touch_hold => |*v, tag| {
                        // Flags for holds must occur before the duration.
                        if (tag == .touch_hold and v.duration != null) return error.InvalidFlagCombination;

                        v.flags.is_fireworks = true;
                        continue :sw self.next() catch break;
                    },
                    .tap, .hold, .slide => return error.InvalidFlagCombination,
                },

                'x' => switch (state) {
                    inline .tap, .hold => |*v, tag| {
                        // Flags for holds must occur before the duration
                        if (tag == .hold and v.duration != null) return error.InvalidFlagCombination;

                        v.flags.is_ex = true;
                        continue :sw self.next() catch break;
                    },
                    .slide, .touch, .touch_hold => return error.InvalidFlagCombination,
                },

                '$' => switch (state) {
                    .tap => |*v| {
                        v.flags.is_star = true;

                        switch (self.next() catch break) {
                            '$' => {
                                v.flags.is_rotating_star = true;
                                continue :sw self.next() catch break;
                            },
                            else => |ch| continue :sw ch,
                        }
                    },
                    .hold, .slide, .touch, .touch_hold => return error.InvalidFlagCombination,
                },

                '[' => {
                    switch (state) {
                        .tap, .touch => return error.UnexpectedDurationStart,
                        .slide => |*slide| slide.time = try self.parseSlideTime(),
                        inline .hold, .touch_hold => |*hold| hold.duration = try self.parseHoldDuration(),
                    }
                    continue :sw self.next() catch break;
                },

                '-', '>', '<', '^', 'v', 'V', 'w', 'p', 'q' => switch (state) {
                    .tap => |v| {
                        var ch = c;
                        const segments = self.parseSlideSegments(&ch, v.pos) catch |e| switch (e) {
                            error.EndOfStream => break :sw,
                            else => return e,
                        };
                        errdefer segments.deinit();

                        state = .{ .slide = .{
                            .pos = v.pos,
                            .segments = segments,
                            .flags = try translateFlags(v.flags, defs.Slide.Flags),
                        } };
                        continue :sw ch;
                    },
                    .touch, .hold, .slide, .touch_hold => return error.IncompatibleNoteTypes,
                },
                else => return error.InvalidCharacter,
            }

            return try state.finalize();
        }

        fn parseSlideSegments(self: *@This(), c: *u8, pos: defs.Position) !std.ArrayList(defs.Slide.Segment) {
            var start = pos;

            var segments = std.ArrayList(defs.Slide.Segment).init(self.allocator);
            errdefer segments.deinit();

            // TODO: parse multiple slides
            while (true) {
                const shape = self.parseSlideShape(c) catch |e| switch (e) {
                    // We hit something that's not a slide character. That's fine.
                    error.InvalidCharacter => break,
                    else => return e,
                };
                const dest_pos = posFromChar(c.*) orelse return error.ExpectedSlideEndPos;

                try shape.validate(start, dest_pos);
                try segments.append(.{ .shape = shape, .dest_pos = dest_pos });

                start = dest_pos;

                c.* = try self.next();
            }

            return segments;
        }

        fn parseHoldDuration(self: *@This()) !defs.Duration {
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
                .duration = undefined,
            };

            const num = try self.parseDecimal(&c);
            switch (c) {
                '#' => {
                    const num2 = try self.parseDecimal(&c);

                    if (with_delay and c == '#') {
                        const delay = num orelse return error.ExpectedDelay;
                        time.delay = delay;

                        if (num2) |_| return error.DuplicateBpmMarkers;

                        const num3 = try self.parseDecimal(&c);
                        if (c == '#') {
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
                        } else {
                            time.duration = try self.parseDuration(null, num3, &c);
                        }
                    } else {
                        const bpm = switch (c) {
                            ':' => num orelse return error.ExpectedBpm,
                            ']' => if (num) |_| return error.ExpectedLength else null,
                            else => return error.InvalidCharacter,
                        };
                        time.duration = try self.parseDuration(bpm, num2, &c);
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

        fn parseDuration(self: *@This(), bpm: ?f32, unit_or_seconds: ?f32, c: *u8) !defs.Duration {
            switch (c.*) {
                ':' => {
                    const unit = unit_or_seconds orelse return error.ExpectedUnit;
                    const length = try self.parseDecimal(c) orelse return error.ExpectedLength;

                    return .{ .measures = .{
                        .bpm = bpm,
                        .unit = unit,
                        .length = length,
                    } };
                },
                ']' => {
                    const seconds = unit_or_seconds orelse return error.ExpectedSeconds;

                    return .{ .seconds = seconds };
                },
                else => return error.InvalidCharacter,
            }
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

fn translateFlags(from: anytype, comptime To: type) !To {
    var t: To = .{};

    outer: inline for (@typeInfo(@TypeOf(from)).@"struct".fields) |from_field| {
        if (@field(from, from_field.name)) {
            inline for (@typeInfo(To).@"struct".fields) |to_field| {
                if (comptime std.mem.eql(u8, from_field.name, to_field.name)) {
                    @field(t, to_field.name) = true;
                    break :outer;
                }
            }

            return error.InvalidFlagCombination;
        }
    }

    return t;
}

const ParseNoteState = union(enum) {
    tap: struct {
        pos: defs.Position,
        flags: defs.Tap.Flags = .{},
    },
    hold: struct {
        pos: defs.Position,
        duration: ?defs.Duration = null,
        flags: defs.Hold.Flags = .{},
    },
    slide: struct {
        pos: defs.Position,
        time: ?SlideTime = null,
        segments: std.ArrayList(defs.Slide.Segment) = undefined,
        flags: defs.Slide.Flags = .{},
    },
    touch: struct {
        area: defs.TouchArea,
        flags: defs.Touch.Flags = .{},
    },
    touch_hold: struct {
        area: defs.TouchArea,
        duration: ?defs.Duration = null,
        flags: defs.TouchHold.Flags = .{},
    },

    const SlideTime = struct {
        delay: ?f32 = null,
        duration: defs.Duration,
    };

    fn finalize(self: @This()) !defs.Note {
        return switch (self) {
            .tap => |tap| .{ .tap = .{
                .pos = tap.pos,
                .flags = tap.flags,
            } },
            .hold => |hold| .{ .hold = .{
                .duration = hold.duration orelse return error.ExpectedDuration,
                .pos = hold.pos,
                .flags = hold.flags,
            } },
            .slide => |slide| {
                const time = slide.time orelse return error.ExpectedDuration;
                return .{ .slide = .{
                    .delay = time.delay,
                    .duration = time.duration,
                    .segments = slide.segments,
                    .start_pos = slide.pos,
                    .flags = slide.flags,
                } };
            },
            .touch => |touch| .{ .touch = .{
                .area = touch.area,
                .flags = touch.flags,
            } },
            .touch_hold => |touch_hold| .{ .touch_hold = .{
                .area = touch_hold.area,
                .duration = touch_hold.duration orelse return error.ExpectedDuration,
                .flags = touch_hold.flags,
            } },
        };
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
        const note = try parser.parse();
        try std.testing.expectEqual(i, note.?.note.tap.pos);
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
            .delay = 1.2,
            .duration = .{ .measures = .{
                .bpm = null,
                .unit = 4.0,
                .length = 3.0,
            } },
        },
        .{
            .delay = 1.2,
            .duration = .{ .seconds = 6.9 },
        },
        .{
            .delay = 1.2,
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
    try std.testing.expectEqual(null, slide.delay);
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

test "invalid flags" {
    const test_cases = [_][]const u8{
        "1f",
        "2$h[2:1]",
        "3hf[2:1]",
        // "4-8x[2:1]",
        // "5-8$[2:1]",
        // "6-8f[2:1]",
        "A7b",
        "B7x",
        "E7$",
        "A8bh",
        "B8hx",
        "E8h$",
    };

    for (test_cases) |case| {
        var parser = initTestParser(case);

        const v = parser.parse();
        try std.testing.expectError(error.InvalidFlagCombination, v);
        try std.testing.expect(v catch null == null);
    }
}
