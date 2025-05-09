module sidero.fileformats.semver.single;
import sidero.base.text;
import sidero.base.errors;

/*
VersionId: [0-9]+

Identifier: ([a-zA-Z0-9-]+(\.[a-zA-Z0-9-]+)*)?
    Start: a-zA-Z0-9-
    Continue: a-zA-Z0-9-
    No leading zeros (max 1)

IdentifierList: IdentifierList . Identifier
                Identifier

Version: v? VersionId (\. VersionId)? (\. VersionId)? (\- IdentifierList)? (\+ IdentifierList)?
*/

///
struct SemVer {
    ///
    uint major, minor, patch;
    ///
    SemVerPreRelease preRelease;
    ///
    String_UTF8 build;

export @safe nothrow @nogc:

    this(return scope ref SemVer other) scope {
        this.tupleof = other.tupleof;
    }

    ~this() scope {
    }

    void opAssign(SemVer other) scope {
        this.__ctor(other);
    }

    ///
    SemVer advancePatch(uint amount) {
        this.patch += amount;

        this.preRelease = SemVerPreRelease.init;
        this.build = String_UTF8.init;

        return this;
    }

    ///
    unittest {
        assert(SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advancePatch(2) == SemVer(1,
                0, 2, SemVerPreRelease.init, String_UTF8.init));
        assert(SemVer(0, 1, 0, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advancePatch(2) == SemVer(0,
                1, 2, SemVerPreRelease.init, String_UTF8.init));
        assert(SemVer(0, 0, 1, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advancePatch(2) == SemVer(0,
                0, 3, SemVerPreRelease.init, String_UTF8.init));
    }

    ///
    SemVer advanceMinor(uint amount) {
        this.minor += amount;
        this.patch = 0;

        this.preRelease = SemVerPreRelease.init;
        this.build = String_UTF8.init;

        return this;
    }

    ///
    unittest {
        assert(SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advanceMinor(2) == SemVer(1,
                2, 0, SemVerPreRelease.init, String_UTF8.init));
        assert(SemVer(0, 1, 0, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advanceMinor(2) == SemVer(0,
                3, 0, SemVerPreRelease.init, String_UTF8.init));
        assert(SemVer(0, 0, 1, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advanceMinor(2) == SemVer(0,
                2, 0, SemVerPreRelease.init, String_UTF8.init));
    }

    ///
    SemVer advanceMajor(uint amount) {
        this.major += amount;
        this.minor = 0;
        this.patch = 0;

        this.preRelease = SemVerPreRelease.init;
        this.build = String_UTF8.init;

        return this;
    }

    ///
    unittest {
        assert(SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advanceMajor(2) == SemVer(3,
                0, 0, SemVerPreRelease.init, String_UTF8.init));
        assert(SemVer(0, 1, 0, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advanceMajor(2) == SemVer(2,
                0, 0, SemVerPreRelease.init, String_UTF8.init));
        assert(SemVer(0, 0, 1, SemVerPreRelease(String_UTF8("prerelease")), String_UTF8("build")).advanceMajor(2) == SemVer(2,
                0, 0, SemVerPreRelease.init, String_UTF8.init));
    }

    ///
    bool opEquals(SemVer other) {
        return this.opCmp(other) == 0;
    }

    ///
    int opCmp(SemVer other) {
        if(this.major < other.major)
            return -1;
        else if(this.major > other.major)
            return 1;

        if(this.minor < other.minor)
            return -1;
        else if(this.minor > other.minor)
            return 1;

        if(this.patch < other.patch)
            return -1;
        else if(this.patch > other.patch)
            return 1;

        return this.preRelease.opCmp(other.preRelease);
    }

    ///
    static Result!SemVer from(String_UTF8 text) {
        return SemVer.from(text);
    }

    ///
    static Result!SemVer from(ref String_UTF8 text) @trusted {
        bool anyMajor, anyMinor, anyPatch;
        Result!SemVer ret = SemVer.from(text, anyMajor, anyMinor, anyPatch);

        if(anyMajor)
            return typeof(return)(MalformedInputException("Invalid Semver spec any major"));
        else if(anyMinor)
            return typeof(return)(MalformedInputException("Invalid Semver spec any minor"));
        else if(anyPatch)
            return typeof(return)(MalformedInputException("Invalid Semver spec any patch"));
        else
            return ret;
    }

    ///
    static Result!SemVer from(ref String_UTF8 text, out bool anyMajor, out bool anyMinor, out bool anyPatch) @trusted {
        const(char)[] textCurrent = text.unsafeGetLiteral, textStart = textCurrent;

        size_t lexNum(out uint num) {
            size_t consumed;

            Loop: foreach(c; textCurrent) {
                switch(c) {
                case '0': .. case '9':
                    num *= 10;
                    num += c - '0';
                    consumed++;
                    break;

                default:
                    break Loop;
                }
            }

            textCurrent = textCurrent[consumed .. $];
            return consumed;
        }

        size_t expect(char c) {
            size_t consumed;

            if(textCurrent.length > 0 && textCurrent[0] == c)
                consumed = 1;

            textCurrent = textCurrent[consumed .. $];
            return consumed;
        }

        size_t lexIdentifierList(ref String_UTF8 list, bool allowLeading0) {
            size_t consumed;

            while(textCurrent.length > 0) {
                size_t consumed2;

                if(!allowLeading0) {
                    uint num;
                    consumed2 = lexNum(num);

                    if(consumed2 > 1 && num == 0)
                        return 0; // leading zeros!
                    consumed += consumed2;
                    consumed2 = 0;
                }

                Loop: foreach(c; textCurrent) {
                    switch(c) {
                    case '0': .. case '9':
                    case 'a': .. case 'z':
                    case 'A': .. case 'Z':
                    case '-':
                        consumed2++;
                        continue Loop;

                    default:
                        break Loop;
                    }
                }

                consumed += consumed2;
                textCurrent = textCurrent[consumed2 .. $];

                if(expect('.'))
                    consumed++;
                else
                    break;
            }

            list = text[0 .. consumed];
            text = text[consumed .. $];
            return consumed;
        }

        SemVer ret;

        expect('v');

        if(expect('*') == 1) {
            anyMajor = true;
            goto After;
        } else if(lexNum(ret.major) == 0) {
            return typeof(return)(MalformedInputException("Invalid Semver spec major"));
        } else if(expect('.') == 0) {
            anyMinor = true;
            goto After;
        } else if(expect('*') == 1) {
            anyMinor = true;
            goto After;
        } else if(lexNum(ret.minor) == 0) {
            return typeof(return)(MalformedInputException("Invalid Semver spec minor"));
        } else if(expect('.') == 0) {
            anyPatch = true;
            goto After;
        } else if(expect('*') == 1) {
            anyPatch = true;
            goto After;
        } else if(lexNum(ret.patch) == 0) {
            return typeof(return)(MalformedInputException("Invalid Semver spec patch"));
        }

    After:
        text = text[textCurrent.ptr - textStart.ptr .. $];

        if(expect('-') == 1) {
            text = text[1 .. $];
            if(lexIdentifierList(ret.preRelease.identifierList, false) == 0)
                return typeof(return)(MalformedInputException("Invalid Semver spec for prerelease"));
        }

        if(expect('+') == 1) {
            text = text[1 .. $];
            if(lexIdentifierList(ret.build, true) == 0)
                return typeof(return)(MalformedInputException("Invalid Semver spec for build"));
        }

        return typeof(return)(ret);
    }
}

///
unittest {
    void test(string text, SemVer expected, string build = "") {
        String_UTF8 text2 = String_UTF8(text);

        bool anyMajor, anyMinor, anyPatch;

        auto got = SemVer.from(text2, anyMajor, anyMinor, anyPatch);
        assert(got);
        assert(got.opEquals(expected));
        assert(got.build == build);
    }

    test("0", SemVer(0, 0, 0));
    test("1", SemVer(1, 0, 0));
    test("0.1", SemVer(0, 1, 0));

    test("0.0.0", SemVer(0, 0, 0));
    test("1.0.0", SemVer(1, 0, 0));
    test("0.1.0", SemVer(0, 1, 0));
    test("0.0.1", SemVer(0, 0, 1));
    test("1.9.0", SemVer(1, 9, 0));
    test("1.10.0", SemVer(1, 10, 0));
    test("1.11.0", SemVer(1, 11, 0));
    test("1.11.2", SemVer(1, 11, 2));
    test("v1.2.3", SemVer(1, 2, 3));

    test("1.0.0-alpha", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("alpha"))));
    test("1.0.0-alpha.1", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("alpha.1"))));
    test("1.0.0-0.3.7", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("0.3.7"))));
    test("1.0.0-x.7.z.92", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("x.7.z.92"))));
    test("1.0.0-x-y-z.--", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("x-y-z.--"))));
    test("1.0.0-alpha.beta", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("alpha.beta"))));
    test("1.0.0-beta.2", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("beta.2"))));
    test("1.0.0-beta.11", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("beta.11"))));
    test("1.0.0-rc.1", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("rc.1"))));

    test("1.0.0-alpha+001", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("alpha"))), "001");
    test("1.0.0+20130313144700", SemVer(1, 0, 0), "20130313144700");
    test("1.0.0-beta+exp.sha.5114f85", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("beta"))), "exp.sha.5114f85");
    test("1.0.0+21AF26D3----117B344092BD", SemVer(1, 0, 0), "21AF26D3----117B344092BD");
}

///
unittest {
    void test(string text, SemVer expected, bool expectedAnyMajor, bool expectedAnyMinor, bool expectedAnyPatch) {
        String_UTF8 text2 = String_UTF8(text);

        bool anyMajor, anyMinor, anyPatch;

        auto got = SemVer.from(text2, anyMajor, anyMinor, anyPatch);
        assert(got);
        assert(anyMajor == expectedAnyMajor);
        assert(anyMinor == expectedAnyMinor);
        assert(anyPatch == expectedAnyPatch);
        assert(got.opEquals(expected));
    }

    test("*", SemVer(0, 0, 0), true, false, false);
    test("1.*", SemVer(1, 0, 0), false, true, false);
    test("1.2.*", SemVer(1, 2, 0), false, false, true);

    test("*-alpha", SemVer(0, 0, 0, SemVerPreRelease(String_UTF8("alpha"))), true, false, false);
    test("1.*-alpha", SemVer(1, 0, 0, SemVerPreRelease(String_UTF8("alpha"))), false, true, false);
    test("1.2.*-alpha", SemVer(1, 2, 0, SemVerPreRelease(String_UTF8("alpha"))), false, false, true);
}

///
struct SemVerPreRelease {
    ///
    String_UTF8 identifierList;

export @safe nothrow @nogc:

    this(return scope ref SemVerPreRelease other) scope {
        this.tupleof = other.tupleof;
    }

    ~this() scope {
    }

    ///
    bool opEquals(SemVerPreRelease other) {
        return this.opCmp(other) == 0;
    }

    ///
    int opCmp(SemVerPreRelease other) @trusted {
        const(char)[] a = this.identifierList.unsafeGetLiteral, b = other.identifierList.unsafeGetLiteral;

        while(a.length > 0 && b.length > 0) {
            SemVerPreReleaseIdentifier ident1 = SemVerPreReleaseIdentifier.from(a), ident2 = SemVerPreReleaseIdentifier.from(b);

            const cmp = ident1.opCmp(ident2);
            if(cmp != 0)
                return cmp;

            const dotA = a.length > 0 && a[0] == '.';
            const dotB = b.length > 0 && b[0] == '.';

            if(dotA)
                a = a[1 .. $];
            if(dotB)
                b = b[1 .. $];

            if((dotA | dotB) == 0)
                break;
        }

        if(a.length > b.length)
            return 1;
        else if(a.length < b.length)
            return -1;
        else
            return 0;
    }
}

///
struct SemVerPreReleaseIdentifier {
    ///
    uint numeric;
    ///
    const(char)[] text;
    ///
    bool isNumeric;

export @safe nothrow @nogc:

    this(return scope ref SemVerPreReleaseIdentifier other) scope {
        this.tupleof = other.tupleof;
    }

    ~this() scope {
    }

    ///
    bool opEquals(SemVerPreReleaseIdentifier other) {
        return this.opCmp(other) == 0;
    }

    ///
    int opCmp(SemVerPreReleaseIdentifier other) {
        import sidero.base.containers.utils;

        if(this.isNumeric && other.isNumeric) {
            if(this.numeric < other.numeric)
                return -1;
            else if(this.numeric > other.numeric)
                return 1;
            else
                return 0;
        } else if(this.isNumeric != other.isNumeric) {
            if(this.isNumeric)
                return -1;
            else
                return 1;
        } else
            return genericCompare(this.text, other.text);
    }

    ///
    static SemVerPreReleaseIdentifier from(ref const(char)[] text) {
        size_t toConsume;
        uint temp;
        bool isNumeric;

        foreach(char c; text) {
            if(c == '.')
                break;
            else if(c < '0' || c > '9')
                isNumeric = false;

            temp *= 10;
            temp += cast(uint)(c - '0');
            toConsume++;
        }

        if(toConsume == 0) {
            // stop
            return SemVerPreReleaseIdentifier.init;
        }

        scope(exit)
            text = text[toConsume .. $];

        if(isNumeric)
            return SemVerPreReleaseIdentifier(temp, null, true);
        return SemVerPreReleaseIdentifier(0, text[0 .. toConsume], false);
    }
}
