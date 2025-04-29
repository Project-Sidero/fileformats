module sidero.fileformats.semver.range;
import sidero.fileformats.semver.single;
import sidero.base.errors;
import sidero.base.text;

/*
https://github.com/npm/node-semver#readme
*/

struct SemVerRange {
    ///
    Mode mode;
    /// More than or equal to start
    SemVer start;
    // Less than to end
    SemVer end;

    ///
    enum Mode {
        /// Has start == end
        Exact,
        /// Have start, minimum of what matches
        Minimum,
        /// Have start + end, inclusive start, exclusive end
        Range,
        /// Matches anything
        Any
    }

export @safe nothrow @nogc:

    this(ref SemVerRange other) scope {
        this.tupleof = other.tupleof;
    }

    ~this() scope {
    }

    void opAssign(ref SemVerRange other) scope {
        this.__ctor(other);
    }

    static Result!SemVerRange from(String_UTF8 text) {
        return SemVerRange.from(text);
    }

    static Result!SemVerRange from(ref String_UTF8 text) @trusted {
        const(char)[] textCurrent = text.unsafeGetLiteral, textStart = textCurrent;

        SemVerRange ret;

        size_t expect(char c) {
            size_t consumed;

            if(textCurrent.length > 0 && textCurrent[0] == c)
                consumed = 1;

            textCurrent = textCurrent[consumed .. $];
            return consumed;
        }

        bool firstMoreThan = cast(bool)expect('>');
        bool firstLessThan = firstMoreThan ? false : cast(bool)expect('<');
        bool firstEqual = cast(bool)expect('=');
        bool firstHasOp = firstMoreThan || firstLessThan || firstEqual;

        bool firstTilde = firstHasOp ? false : cast(bool)expect('~');
        firstHasOp = firstHasOp || firstTilde;

        bool firstCaret = firstHasOp ? false : cast(bool)expect('^');
        firstHasOp = firstHasOp || firstCaret;

        bool firstHasAny;
        bool firstAnyMinor, firstAnyPatch;

        {
            text = text[textCurrent.ptr - textStart.ptr .. $];
            textStart = textCurrent;

            bool anyMajor;
            auto got = SemVer.from(text, anyMajor, firstAnyMinor, firstAnyPatch);

            if(!got)
                return typeof(return)(got.getError);

            ret.start = got;
            firstHasAny = anyMajor || firstAnyMinor || firstAnyPatch;

            // handle *
            // the >= prefix don't mean anything here, ignore it

            if(anyMajor) {
                ret.mode = SemVerRange.Mode.Any;
                return typeof(return)(ret);
            } else {
                textCurrent = text.unsafeGetLiteral;
                textStart = textCurrent;
            }

            if(firstMoreThan && !firstEqual) {
                // > prefix
                // >1.* becomes >=2.0.0
                // >1.0.* becomes >= 1.1.0
                // >1.0.0 becomes >= 1.0.1

                if(firstAnyMinor)
                    ret.start.advanceMajor(1);
                else if(firstAnyPatch)
                    ret.start.advanceMinor(1);
                else
                    ret.start.advancePatch(1);
            } else if(firstLessThan) {
                // < prefix
                // <= prefix
                // <=1.0.0 becomes >=0.0.0 <1.0.1
                // <=1.0.* becomes >=0.0.0 <1.1.0
                // <=1.* becomes >=0.0.0 <2.0.0

                ret.mode = SemVerRange.Mode.Range;
                ret.end = ret.start;
                ret.start = SemVer(0, 0, 0);

                if(firstAnyMinor)
                    ret.end.advanceMajor(1);
                else if(firstAnyPatch)
                    ret.end.advanceMinor(1);
                else if(firstEqual)
                    ret.end.advancePatch(1);

                return typeof(return)(ret);
            } else if(!firstMoreThan && firstEqual) {
                // = prefix
                // =1.* becomes >=1.0.0 <2.0.0
                // =1.0.* becomes >=1.0.0 <1.1.0

                if(firstAnyMinor) {
                    ret.mode = SemVerRange.Mode.Range;
                    ret.end = ret.start;
                    ret.end.advanceMajor(1);
                } else if(firstAnyPatch) {
                    ret.mode = SemVerRange.Mode.Range;
                    ret.end = ret.start;
                    ret.end.advanceMinor(1);
                } else
                    ret.mode = SemVerRange.Mode.Exact;

                return typeof(return)(ret);
            } else if(firstTilde) {
                // ~ prefix
                // ~1 becomes >= 1.0.0 < 2.0.0
                // ~0.1 becomes >= 0.1.0 < 0.2.0
                // ~0.0.1 becomes >= 0.0.1 < 0.1.0

                ret.mode = SemVerRange.Mode.Range;
                ret.end = ret.start;

                if(firstAnyMinor)
                    ret.end.advanceMajor(1);
                else
                    ret.end.advanceMinor(1);

                return typeof(return)(ret);
            } else if (firstCaret) {
                // ^ prefix
                // ^1.2.3 becomes >=1.2.3 <2.0.0
                // ^0.2.3 becomes >=0.2.3 <0.3.0
                // ^0.0.3 becomes >=0.0.3 <0.0.4

                ret.mode = SemVerRange.Mode.Range;
                ret.end = ret.start;

                if (ret.end.major == 0) {
                    if (ret.end.minor == 0) {
                        ret.end.advancePatch(1);
                    } else {
                        ret.end.advanceMinor(1);
                    }
                } else
                    ret.end.advanceMajor(1);

                return typeof(return)(ret);
            }

            // At this point we have the minimum case covered,
            //  it could be: exact, minimum or range,
            //  can only tell the difference based upon the next couple of characters.
        }

        {
            // " - " indicates ranges
            // firstMoreThan indicates minimum if not a range
            // otherwise exact

            if(expect(' ') == 1) {
                // required no matter the syntax

                bool rangeOp = cast(bool)expect('-');
                bool lessThanOp = rangeOp ? false : cast(bool)expect('<');
                bool equalOp = rangeOp ? false : cast(bool)expect('=');
                expect(' ');

                if(rangeOp || lessThanOp || equalOp) {
                    // ok, this is a range

                    text = text[textCurrent.ptr - textStart.ptr .. $];
                    textStart = textCurrent;

                    bool anyMajor, anyMinor, anyPatch;
                    auto got = SemVer.from(text, anyMajor, anyMinor, anyPatch);

                    if(!got)
                        return typeof(return)(got.getError);
                    ret.end = got;

                    if(anyMajor) {
                        ret.mode = SemVerRange.Mode.Minimum;
                        return typeof(return)(ret);
                    } else if(rangeOp || (lessThanOp && equalOp)) {
                        if(anyMinor)
                            ret.end.advanceMajor(1);
                        else if(anyPatch)
                            ret.end.advanceMinor(1);
                        else
                            ret.end.advancePatch(1);
                    }

                    ret.mode = SemVerRange.Mode.Range;
                    return typeof(return)(ret);
                }
            }

            textStart = textCurrent;
        }

        if(firstHasAny && !firstHasOp) {
            // Have an any, that means we have a range implicitly
            ret.mode = SemVerRange.Mode.Range;
            ret.end = ret.start;

            if(firstAnyMinor)
                ret.end.advanceMajor(1);
            else if(firstAnyPatch)
                ret.end.advanceMinor(1);

            return typeof(return)(ret);
        } else if(firstMoreThan) {
            ret.mode = SemVerRange.Mode.Minimum;
            return typeof(return)(ret);
        } else {
            ret.mode = SemVerRange.Mode.Exact;
            return typeof(return)(ret);
        }
    }
}

///
unittest {
    alias Mode = SemVerRange.Mode;

    void test(string text, Mode mode, SemVer start, SemVer end = SemVer.init) {
        auto got = SemVerRange.from(String_UTF8(text));
        assert(got);

        assert(got.mode == mode);
        assert(got.start == start);
        assert(got.end == end);
    }

    test("*", Mode.Any, SemVer.init);
    test("1.*", Mode.Range, SemVer(1, 0, 0), SemVer(2, 0, 0));
    test("1.2.*", Mode.Range, SemVer(1, 2, 0), SemVer(1, 3, 0));
    test("1.2.3", Mode.Exact, SemVer(1, 2, 3));

    test("=1", Mode.Range, SemVer(1, 0, 0), SemVer(2, 0, 0));
    test("=1.*", Mode.Range, SemVer(1, 0, 0), SemVer(2, 0, 0));
    test("=1.2", Mode.Range, SemVer(1, 2, 0), SemVer(1, 3, 0));
    test("=1.2.*", Mode.Range, SemVer(1, 2, 0), SemVer(1, 3, 0));
    test("=1.2.3", Mode.Exact, SemVer(1, 2, 3));

    test(">1", Mode.Minimum, SemVer(2, 0, 0));
    test(">1.*", Mode.Minimum, SemVer(2, 0, 0));
    test(">1.2", Mode.Minimum, SemVer(1, 3, 0));
    test(">1.2.*", Mode.Minimum, SemVer(1, 3, 0));
    test(">1.2.3", Mode.Minimum, SemVer(1, 2, 4));

    test(">=1", Mode.Minimum, SemVer(1, 0, 0));
    test(">=1.*", Mode.Minimum, SemVer(1, 0, 0));
    test(">=1.2", Mode.Minimum, SemVer(1, 2, 0));
    test(">=1.2.*", Mode.Minimum, SemVer(1, 2, 0));
    test(">=1.2.3", Mode.Minimum, SemVer(1, 2, 3));

    test("<1", Mode.Range, SemVer(0, 0, 0), SemVer(2, 0, 0));
    test("<1.*", Mode.Range, SemVer(0, 0, 0), SemVer(2, 0, 0));
    test("<1.2", Mode.Range, SemVer(0, 0, 0), SemVer(1, 3, 0));
    test("<1.2.*", Mode.Range, SemVer(0, 0, 0), SemVer(1, 3, 0));
    test("<1.2.3", Mode.Range, SemVer(0, 0, 0), SemVer(1, 2, 3));

    test("<=1", Mode.Range, SemVer(0, 0, 0), SemVer(2, 0, 0));
    test("<=1.*", Mode.Range, SemVer(0, 0, 0), SemVer(2, 0, 0));
    test("<=1.2", Mode.Range, SemVer(0, 0, 0), SemVer(1, 3, 0));
    test("<=1.2.*", Mode.Range, SemVer(0, 0, 0), SemVer(1, 3, 0));
    test("<=1.2.3", Mode.Range, SemVer(0, 0, 0), SemVer(1, 2, 4));

    test("1 - 2.3.4", Mode.Range, SemVer(1, 0, 0), SemVer(2, 3, 5));
    test("1.2 - 2.3.4", Mode.Range, SemVer(1, 2, 0), SemVer(2, 3, 5));
    test("1.2.3 - 2.3.4", Mode.Range, SemVer(1, 2, 3), SemVer(2, 3, 5));
    test("1.2.3 - 2.3", Mode.Range, SemVer(1, 2, 3), SemVer(2, 4, 0));
    test("1.2.3 - 2", Mode.Range, SemVer(1, 2, 3), SemVer(3, 0, 0));

    test(">1 <3", Mode.Range, SemVer(2, 0, 0), SemVer(3, 0, 0));
    test(">1.* <3", Mode.Range, SemVer(2, 0, 0), SemVer(3, 0, 0));
    test(">1.2 <3", Mode.Range, SemVer(1, 3, 0), SemVer(3, 0, 0));
    test(">1.2.* <3", Mode.Range, SemVer(1, 3, 0), SemVer(3, 0, 0));
    test(">1.2.3 <3", Mode.Range, SemVer(1, 2, 4), SemVer(3, 0, 0));

    test(">=1 <3", Mode.Range, SemVer(1, 0, 0), SemVer(3, 0, 0));
    test(">=1.* <3", Mode.Range, SemVer(1, 0, 0), SemVer(3, 0, 0));
    test(">=1.2 <3", Mode.Range, SemVer(1, 2, 0), SemVer(3, 0, 0));
    test(">=1.2.* <3", Mode.Range, SemVer(1, 2, 0), SemVer(3, 0, 0));
    test(">=1.2.3 <3", Mode.Range, SemVer(1, 2, 3), SemVer(3, 0, 0));

    test("~1", Mode.Range, SemVer(1, 0, 0), SemVer(2, 0, 0));
    test("~1.*", Mode.Range, SemVer(1, 0, 0), SemVer(2, 0, 0));
    test("~1.2", Mode.Range, SemVer(1, 2, 0), SemVer(1, 3, 0));
    test("~1.2.*", Mode.Range, SemVer(1, 2, 0), SemVer(1, 3, 0));
    test("~1.2.3", Mode.Range, SemVer(1, 2, 3), SemVer(1, 3, 0));

    test("^1.2.3", Mode.Range, SemVer(1, 2, 3), SemVer(2, 0, 0));
    test("^0.2.3", Mode.Range, SemVer(0, 2, 3), SemVer(0, 3, 0));
    test("^0.0.3", Mode.Range, SemVer(0, 0, 3), SemVer(0, 0, 4));
}
