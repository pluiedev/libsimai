const std = @import("std");

const Note = union(enum) {
    rest: Unit,
    tap: Tap,
    hold: Hold,
    slide: Slide,
    touch: Touch,
    touch_hold: TouchHold,
    each: []Note,
};

// There are only 8 spots on the judgement line.
const Position = u3;

const Unit = u32;

const Duration = union(enum) {
    measures: struct {
        bpm: ?f32,
        unit: Unit,
        length: u32,
    },
    seconds: f32,
};

const Tap = struct {
    unit: Unit,
    /// Position of the tap note on screen.
    pos: Position,
    /// Whether the tap note is a break note, colored orange.
    /// Break notes are only worth their full value when hit at the critical perfect
    /// window, which is extra precise than a normal perfect.
    is_break: bool,
    /// Whether the tap note should appear as a star (like the head of a slide)
    /// instead of a circle note.
    is_star: bool,
    /// Whether the tap note is an EX note, which is judged as a critical perfect
    /// if you hit anywhere within the "good" judgement window.
    is_ex: bool,
};
const Hold = struct {
    /// The duration for which the hold note should be held.
    duration: Duration,
    /// Position of the hold note on screen.
    pos: Position,
    /// Whether the hold note is a "break" note, colored orange.
    /// Break notes are only worth their full value when hit at the critical perfect
    /// window, which is extra precise than a normal perfect.
    is_break: bool,
    /// Whether the hold note is an EX note, which is judged as a critical perfect
    /// if you hit anywhere within the "good" judgement window.
    is_ex: bool,
};

// TODO: implement mutiple slides & chaining slides
const Slide = struct {
    /// The amount of delay between the slide head (a tap note) reaches the judgement line
    /// and its trail actually starts moving.
    delay: Delay,
    /// The duration for which the slide note should appear on the screen,
    duration: Duration,
    /// Start position of the slide note on screen.
    start_pos: Position,
    /// End position of the slide note on screen.
    end_pos: Position,
    /// The shape of the slide note.
    shape: SlideShape,
    /// Whether the slide note is a "break" note, colored orange.
    /// Break notes are only worth their full value when hit at the critical perfect
    /// window, which is extra precise than a normal perfect.
    is_break: bool,

    const Delay = union(enum) {
        one_beat_at_bpm: ?f32,
        seconds: f32,
    };
};
const Touch = struct {
    /// The time when the touch note occurs.
    time: f32,
    /// Position of the touch note on screen.
    area: TouchArea,
    /// Whether the touch note emits firework effects when hit.
    is_fireworks: bool,
};
const TouchHold = struct {
    /// The time when the touch hold note occurs.
    time: f32,
    /// The duration for which the touch hold note should be held.
    duration: f32,
    /// Position of the touch hold note on screen.
    /// Official charts have only ever used the center (C) area, but the Simai format
    /// allows it to be placed anywhere a regular tap note can be placed at.
    area: TouchArea,
    /// Whether the touch hold emits firework effects when hit.
    is_fireworks: bool,
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

    pub fn isValid(self: @This(), start: Position, end: Position) bool {
        // Used to simplify certain symmetric patterns
        const dist = @This().posDist(start, end);
        const neighboringEndpoints = dist == 1;
        const oppositeEndpoints = dist == 4;

        return switch (self) {
            .straight => start != end and !neighboringEndpoints,
            .arc => |dir| arc: {
                // If the arc has a direction, anything is allowed.
                if (dir) |_| return true;

                // Short arcs must not start and end at the same place,
                // and must not connect two opposite positions on the screen
                // (that would be a straight line)
                break :arc start != end and !oppositeEndpoints;
            },
            .loop => |_| true,
            .grand_loop => |_| true,
            .thunderbolt => |_| oppositeEndpoints,

            // Disallow opposite end points because that would be a straight line.
            .v => start != end and !oppositeEndpoints,

            .grand_v => |middle| grand_v: {
                const dist1 = @This().posDist(start, middle);
                // Middle point has to be exactly two spots away from start point.
                if (dist1 != 2) return false;

                const dist2 = @This().posDist(middle, end);

                // Second leg must be longer than the first leg.
                break :grand_v dist2 > dist1;
            },
            .fan => oppositeEndpoints,
        };
    }

    fn posDist(p1: Position, p2: Position) u3 {
        const dist = @max(p1, p2) - @min(p1, p2);
        // Subtraction led to the longer path being chosen;
        // we adjust that to give the shorter path
        return if (dist > 4) dist - 4 else dist;
    }

    test {
        std.testing.expectEqual(3, posDist(8, 3));
    }
};

const TouchArea = union(enum) {
    /// Touch areas on the dots on the judgement line.
    a: Position,
    /// Touch areas that lie on the midpoint between the judgement line dots and the center.
    b: Position,
    /// The touch area that lies in the center.
    c,
    /// Touch areas *between* the dots on the judgement line.
    d: Position,
    /// Touch areas that lie on the line between the D touch areas and the center.
    /// Slightly more outwards than B touch areas.
    e: Position,
};
