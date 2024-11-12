module sidero.fileformats.errors;
import sidero.fileformats.lexers.defs : Loc;
import sidero.base.allocators.classes;
import sidero.base.text;
import sidero.base.logger;

///
alias ErrorSinkRef = CRef!(ErrorSink!());
///
alias ErrorSinkRef_Console = CRef!(ErrorSink_Console!());
///
alias ErrorSinkRef_Logger = CRef!(ErrorSink_Console!());

///
export extern (C++) class ErrorSink() : RootRefRCClass!() {
    bool haveError;
    bool gagged;

export @safe nothrow @nogc:

    ///
    void error(Loc location, String_UTF8 message) {
        this.haveError = true;
    }

    ///
    void errorSupplimental(String_UTF8 message) {
        this.haveError = true;
    }

final extern(D):

    ///
    void error(Loc location, string message) {
        this.error(location, String_UTF8(message));
    }

    ///
    void errorSupplimental(string message) {
        this.errorSupplimental(String_UTF8(message));
    }

    ///
    void error(Args...)(Loc location, string format, Args args) if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.error(location, text);
    }

    ///
    void errorSupplimental(Args...)(string format, Args args) if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.errorSupplimental(text);
    }

    ///
    void error(Args...)(Loc location, String_UTF8 format, Args args) if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.error(location, text);
    }

    ///
    void errorSupplimental(Args...)(String_UTF8 format, Args args) if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.errorSupplimental(text);
    }
}

///
export extern (C++) class ErrorSink_Console() : RootRefRCClass!() {
    import sidero.base.console;

    bool haveError;
    bool gagged;

export @safe nothrow @nogc:

    ///
    void error(Loc location, String_UTF8 message) {
        this.haveError = true;

        if(!gagged)
            writeln(useErrorStream(true), location.fileName, ":", location.lineNumber, ": error: ", message);
    }

    ///
    void errorSupplimental(String_UTF8 message) {
        this.haveError = true;
        if(!gagged)
            writeln(useErrorStream(true), "    ", message);
    }
}

///
export extern (C++) class ErrorSink_Logger() : RootRefRCClass!() {
    bool haveError;
    bool gagged;

    LoggerReference logger;

export @safe nothrow @nogc:

    this(String_UTF8 name) {
        logger = Logger.forName(name);
        assert(logger);
    }

    ///
    void error(Loc location, String_UTF8 message) {
        this.haveError = true;

        if(!gagged)
            logger.error(location.fileName, ":", location.lineNumber, ": error: ", message);
    }

    ///
    void errorSupplimental(String_UTF8 message) {
        this.haveError = true;
        if(!gagged)
            logger.error("    ", message);
    }
}
