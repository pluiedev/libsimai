const std = @import("std");

pub const Event = union(enum) {
    /// A note.
    note: Note,

    /// Sets the effective BPM for the subsequent notes.
    set_bpm: f32,

    /// Sets the subdivision length per comma for the subsequent notes.
    /// For example, 4 specifies a comma to have a value of a quarter note
    /// 2 a half note, 1 a whole note, etc.
    /// Could be a decimal number but that should be avoided.
    set_unit: Unit,

    /// The end point of the chart.
    end,
};

pub const Note = union(enum) {
    rest,
    tap: Tap,
    hold: Hold,
    slide: Slide,
    touch: Touch,
    touch_hold: TouchHold,
    each: std.ArrayList(Note),

    fn deinit(self: @This()) void {
        switch (self) {
            .slide => |slide| slide.segments.deinit(),
            .each => |each| {
                for (each) |e| e.deinit();
                each.deinit();
            },
        }
    }
};

// There are only 8 spots on the judgement line.
pub const Position = u3;

pub const Unit = f32;

pub const Duration = union(enum) {
    measures: struct {
        bpm: ?f32,
        unit: Unit,
        length: f32,
    },
    seconds: f32,
};

pub const Tap = struct {
    /// Position of the tap note on screen.
    pos: Position,
    flags: Flags = .{},

    pub const Flags = packed struct {
        /// Whether the tap note is a break note, colored orange.
        /// Break notes are only worth their full value when hit at the critical perfect
        /// window, which is extra precise than a normal perfect.
        is_break: bool = false,
        /// Whether the tap note is an EX note, which is judged as a critical perfect
        /// if you hit anywhere within the "good" judgement window.
        is_ex: bool = false,
        /// Whether the tap note should appear as a star (like the head of a slide)
        /// instead of a circle note.
        is_star: bool = false,
        /// Whether the star-shaped tap note should rotate.
        is_rotating_star: bool = false,
    };
};
pub const Hold = struct {
    /// The duration for which the hold note should be held.
    duration: Duration,
    /// Position of the hold note on screen.
    pos: Position,
    flags: Flags = .{},

    pub const Flags = packed struct {
        /// Whether the hold note is a "break" note, colored orange.
        /// Break notes are only worth their full value when hit at the critical perfect
        /// window, which is extra precise than a normal perfect.
        is_break: bool = false,
        /// Whether the hold note is an EX note, which is judged as a critical perfect
        /// if you hit anywhere within the "good" judgement window.
        is_ex: bool = false,
    };
};

// TODO: implement mutiple slides & chaining slides
pub const Slide = struct {
    /// The amount of delay between the slide head (a tap note) reaches the judgement line
    /// and its trail actually starts moving.
    /// If null, defaults to one beat at the specified/default BPM.
    delay: ?f32,
    /// The duration for which the slide note should appear on the screen,
    duration: Duration,
    /// Start position of the slide note on screen.
    start_pos: Position,
    /// The list of segments in the slide note.
    segments: std.ArrayList(Segment),
    flags: Flags = .{},

    pub const Segment = struct {
        /// The shape of the segment.
        shape: Shape,

        /// The destination of the segment.
        dest_pos: Position,
    };

    pub const Shape = SlideShape;

    pub const Flags = packed struct {
        /// Whether the slide note is a "break" note, colored orange.
        /// Break notes are only worth their full value when hit at the critical perfect
        /// window, which is extra precise than a normal perfect.
        is_break: bool = false,
    };
};
pub const Touch = struct {
    /// Position of the touch note on screen.
    area: TouchArea,
    flags: Flags = .{},

    pub const Flags = packed struct {
        /// Whether the touch note emits firework effects when hit.
        is_fireworks: bool = false,
    };
};
pub const TouchHold = struct {
    /// The duration for which the touch hold note should be held.
    duration: Duration,
    /// Position of the touch hold note on screen.
    /// Official charts have only ever used the center (C) area, but the Simai format
    /// allows it to be placed anywhere a regular tap note can be placed at.
    area: TouchArea,
    flags: Flags = .{},

    pub const Flags = packed struct {
        /// Whether the touch hold note emits firework effects when hit.
        is_fireworks: bool = false,
    };
};

const SlideShape = union(enum) {
    /// A slide that connects the start and end points in a straight line.
    straight,

    /// A slide that forms an arc in the middle of the screen from the start to the end point.
    /// Arcs can be clockwise or anticlockwise, though for short arcs that only pass through
    /// one half of a screen (e.g. a u-turn), the direction can be unset.
    arc: ?Direction,

    /// A slide that forms a loop around the center from the start to the end point.
    /// Also known as p-shape (clockwise) and q-shape (anti-clockwise).
    loop: ?Direction,

    /// A slide that forms a loop around "an imaginary circle that is tangent to both
    /// the screen center and the circular judgement line", per Simai Wiki.
    /// Also known as grand p-shape (clockwise) and grand q-shape (anti-clockwise).
    grand_loop: ?Direction,

    /// A slide that consists of three line segments connecting the start and end points
    /// like a thunderbolt.
    /// Also known as z-shape (clockwise) and s-shape (anti-clockwise).
    thunderbolt: ?Direction,

    /// A slide that connects the start and end points via two line segments through the center.
    v,

    /// A slide that connects the start and end points via some middle point.
    /// The line segment between the start point and the middle point is always shorter
    /// than the line segment between the middle point and the end point.
    grand_v: Position,

    /// A slide shape that spreads the end point to cover the two adjacent points as well,
    /// resembling a folding fan.
    fan,

    const Direction = enum {
        clockwise,
        anticlockwise,
    };

    pub fn validate(self: @This(), start: Position, end: Position) !void {
        // Used to simplify certain symmetric patterns
        const dist = posDist(start, end);
        const neighboringEndpoints = dist == 1;
        const oppositeEndpoints = dist == 4;

        return switch (self) {
            .straight => {
                if (start == end) return error.EndpointsMustBeSeparate;
                if (neighboringEndpoints) return error.EndpointsMustNotBeNeighbors;
            },
            .arc => |dir| {
                // If the arc has a direction, anything is allowed.
                if (dir) |_| return;

                // Short arcs must not start and end at the same place,
                // and must not connect two opposite positions on the screen
                // (that would be a straight line)
                if (start == end) return error.EndpointsMustBeSeparate;
                if (oppositeEndpoints) return error.EndpointsMustNotBeOpposite;
            },
            .loop => |_| return,
            .grand_loop => |_| return,
            .thunderbolt => |_| {
                if (!oppositeEndpoints) return error.EndpointsMustBeOpposite;
            },

            .v => {
                // Disallow opposite end points because that would be a straight line.
                if (start == end) return error.EndpointsMustBeSeparate;
                if (oppositeEndpoints) return error.EndpointsMustNotBeOpposite;
            },

            .grand_v => |middle| {
                const dist1 = posDist(start, middle);
                // Middle point has to be exactly two spots away from start point.
                if (dist1 != 2) return error.MiddlePointMustBeTwoPositionsAway;

                const dist2 = posDist(middle, end);

                // Second leg must be longer than the first leg.
                if (dist1 > dist2) return error.SecondLegMustBeLongerThanTheFirst;
            },
            .fan => {
                if (!oppositeEndpoints) return error.EndpointsMustBeOpposite;
            },
        };
    }

    fn posDist(p1: Position, p2: Position) u3 {
        const dist = @max(p1, p2) - @min(p1, p2);
        // std.debug.print("{}\n", .{(dist ^ 0b111) + 1});
        // Subtraction led to the longer path being chosen;
        // we adjust that to give the shorter path
        return if (dist > 4) (dist ^ std.math.maxInt(Position)) +% 1 else dist;
    }

    test {
        try std.testing.expectEqual(3, posDist(7, 2));
        try std.testing.expectEqual(1, posDist(7, 0));
    }
};

pub const TouchArea = union(enum) {
    /// Touch areas on the dots on the judgement line.
    a: Position,
    /// Touch areas that lie on the midpoint between the judgement line dots and the center.
    b: Position,
    /// The two touch areas that lies in the center.
    c: ?u1,
    /// Touch areas *between* the dots on the judgement line.
    d: Position,
    /// Touch areas that lie on the line between the D touch areas and the center.
    /// Slightly more outwards than B touch areas.
    e: Position,
};
