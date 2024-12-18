module sidero.fileformats.errors;
import sidero.fileformats.lexers.defs : Loc;
import sidero.base.allocators.classes;
import sidero.base.synchronization.system.lock;
import sidero.base.text;
import sidero.base.internal.logassert;

///
alias ErrorSinkRef = CRef!(ErrorSink!());
///
alias ErrorSinkRef_Console = CRef!(ErrorSink_Console!());
///
alias ErrorSinkRef_Logger = CRef!(ErrorSink_Console!());
///
alias ErrorSinkRef_StringBuilder = CRef!(ErrorSink_StringBuilder!());

///
export extern (C++) class ErrorSink() : RootRefRCClass!() {
    bool haveError;
    bool gagged;

    private SystemLock mutex;

export @safe nothrow @nogc:

    ///
    void error(Loc location, String_UTF8 message) scope {
        logAssert(cast(bool)mutex.lock, "Failed to lock");
        scope(exit)
            mutex.unlock;

        this.haveError = true;
    }

    ///
    void errorSupplimental(String_UTF8 message) scope {
        logAssert(cast(bool)mutex.lock, "Failed to lock");
        scope(exit)
            mutex.unlock;

        this.haveError = true;
    }

final extern (D):

    ///
    void error(Loc location, string message) scope {
        this.error(location, String_UTF8(message));
    }

    ///
    void errorSupplimental(string message) scope {
        this.errorSupplimental(String_UTF8(message));
    }

    ///
    void error(Args...)(Loc location, string format, Args args) scope if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.error(location, text);
    }

    ///
    void errorSupplimental(Args...)(string format, Args args) scope if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.errorSupplimental(text);
    }

    ///
    void error(Args...)(Loc location, String_UTF8 format, Args args) scope if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.error(location, text);
    }

    ///
    void errorSupplimental(Args...)(String_UTF8 format, Args args) scope if (Args.length > 0) {
        String_UTF8 text = formattedWrite(format, args).asReadOnly();
        this.errorSupplimental(text);
    }
}

///
export extern (C++) class ErrorSink_Console() : ErrorSink!() {
    import sidero.base.console;

export @safe nothrow @nogc:

    ///
    override void error(Loc location, String_UTF8 message) scope {
        if(!gagged) {
            logAssert(cast(bool)mutex.lock, "Failed to lock");
            scope(exit)
                mutex.unlock;

            this.haveError = true;
            writeln(useErrorStream(true), location.fileName, ":", location.lineNumber, ": error: ", message);
        }
    }

    ///
    override void errorSupplimental(String_UTF8 message) scope {
        if(!gagged) {
            logAssert(cast(bool)mutex.lock, "Failed to lock");
            scope(exit)
                mutex.unlock;

            this.haveError = true;
            writeln(useErrorStream(true), "    ", message);
        }
    }
}

///
export extern (C++) class ErrorSink_Logger() : ErrorSink!() {
    import sidero.base.logger;

    LoggerReference logger;

export @safe nothrow @nogc:

    ///
    this(String_UTF8 name) scope {
        logger = Logger.forName(name);
        assert(logger);
    }

    ///
    this(LoggerReference logger) scope {
        this.logger = logger;
        assert(logger);
    }

    ///
    override void error(Loc location, String_UTF8 message) scope {
        if(!gagged) {
            logAssert(cast(bool)mutex.lock, "Failed to lock");
            scope(exit)
                mutex.unlock;

            this.haveError = true;
            logger.error(location.fileName, ":", location.lineNumber, ": error: ", message);
        }
    }

    ///
    override void errorSupplimental(String_UTF8 message) scope {
        if(!gagged) {
            logAssert(cast(bool)mutex.lock, "Failed to lock");
            scope(exit)
                mutex.unlock;

            this.haveError = true;
            logger.error("    ", message);
        }
    }
}

///
export extern (C++) class ErrorSink_StringBuilder() : ErrorSink!() {
    StringBuilder_UTF8 builder;

export @safe nothrow @nogc:

    ///
    override void error(Loc location, String_UTF8 message) scope {
        if(!gagged) {
            logAssert(cast(bool)mutex.lock, "Failed to lock");
            scope(exit)
                mutex.unlock;

            this.haveError = true;
            builder.formattedWrite(String_UTF8("{:s}:{:d}: error: {:s}\n"), location.fileName, location.lineNumber, message);
        }
    }

    ///
    override void errorSupplimental(String_UTF8 message) scope {
        if(!gagged) {
            logAssert(cast(bool)mutex.lock, "Failed to lock");
            scope(exit)
                mutex.unlock;

            this.haveError = true;

            builder ~= "    ";
            builder ~= message;
            builder ~= "\n";
        }
    }
}
