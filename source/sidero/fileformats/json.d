module sidero.fileformats.json;
import sidero.base.allocators;
import sidero.base.text;
import sidero.base.math.bigint;
import sidero.base.containers.dynamicarray;
import sidero.base.containers.readonlyslice;
import sidero.base.containers.list.linkedlist;
import sidero.base.containers.map.hashmap;
import sidero.base.attributes;

/// Wrapper around the types that JSON supports as a value, along with comments.
struct JSONValue {
    package(sidero.fileformats) @PrettyPrintIgnore @hidden {
        JSONState* node;
    }

export @safe nothrow @nogc:

    this(return scope ref JSONValue other) scope {
        this.node = other.node;

        if(this.node !is null)
            this.node.rc(true);
    }

    ~this() scope {
        if(this.node !is null)
            this.node.rc(false);
    }

    ///
    bool isNull() scope const {
        return this.node is null;
    }

    ///
    Type type() scope const {
        if(isNull)
            return Type.Null;
        else
            return this.node.type;
    }

    ///
    void nullify() scope {
        if(isNull)
            return;

        this.resetTo(Type.Null);
    }

    ///
    void opAssign(return scope JSONValue other) scope @trusted {
        this.destroy;
        this.__ctor(other);
    }

    ///
    void opAssign(return scope ref JSONValue other) scope @trusted {
        this.destroy;
        this.__ctor(other);
    }

    ///
    void opAssign(bool value) scope {
        if(isNull)
            return;

        this.resetTo(Type.Boolean);
        this.node.boolean = value;
    }

    ///
    void opAssign(double value) scope {
        if(isNull)
            return;

        this.resetTo(Type.Number);
        this.node.number = value;
    }

    ///
    void opAssign(DynamicBigInteger value) scope @trusted {
        if(isNull)
            return;

        this.resetTo(Type.BigInteger);
        this.node.bigInteger = value;
    }

    ///
    void opAssign(String_UTF8 value) scope @trusted {
        if(isNull)
            return;

        this.resetTo(Type.Text);
        this.node.text = value;
    }

    ///
    void opAssign(DynamicArray!JSONValue value) scope {
        this.opAssign(value.asReadOnly());
    }

    ///
    void opAssign(Slice!JSONValue value) scope @trusted {
        if(isNull)
            return;

        this.resetTo(Type.Array);
        this.node.array ~= value;
    }

    ///
    void opAssign(LinkedList!JSONValue value) scope @trusted {
        if(isNull)
            return;

        this.resetTo(Type.Array);
        this.node.array ~= value;
    }

    ///
    void opAssign(HashMap!(String_UTF8, JSONValue) value) scope @trusted {
        if(isNull)
            return;

        this.resetTo(Type.Object);
        this.node.obj = value;
    }

    ///
    void resetTo(Type type) scope @trusted {
        import sidero.base.allocators.utils;

        if(isNull)
            return;

        this.node.cleanup();
        this.node.type = type;

        final switch(type) {
        case Type.Null:
            break;

        case Type.Array:
            fillUninitializedWithInit(this.node.array);
            this.node.array = LinkedList!JSONValue(this.node.allocator);
            break;
        case Type.Object:
            fillUninitializedWithInit(this.node.obj);
            this.node.obj = HashMap!(String_UTF8, JSONValue)(this.node.allocator);
            break;
        case Type.BigInteger:
            fillUninitializedWithInit(this.node.bigInteger);
            this.node.bigInteger = DynamicBigInteger(0);
            break;
        case Type.Boolean:
            this.node.boolean = false;
            break;
        case Type.Number:
            this.node.number = double.nan;
            break;
        case Type.Text:
            fillUninitializedWithInit(this.node.text);
            this.node.text = String_UTF8.init;
            break;
        }
    }

    ///
    bool haveAttachedComments() scope {
        if(isNull)
            return false;
        else
            return this.node.attachedComments.length > 0;
    }

    ///
    DynamicArray!String_UTF8 attachedComments() return scope {
        if(isNull)
            return typeof(return).init;

        if(this.node.attachedComments.isNull)
            this.node.attachedComments = DynamicArray!String_UTF8(this.node.allocator);

        return this.node.attachedComments;
    }

    package(sidero.fileformats) {
        void setOrAddAttachedComments(DynamicArray!String_UTF8 ac) scope {
            if(isNull || ac.length == 0)
                return;

            if(this.node.attachedComments.length > 0)
                ac ~= this.node.attachedComments;

            this.node.attachedComments = ac;
        }
    }

    ///
    void match(scope void delegate(LinkedList!JSONValue) @safe nothrow @nogc arrayDel,
            scope void delegate(HashMap!(String_UTF8, JSONValue)) @safe nothrow @nogc objectDel, scope void delegate(
                DynamicBigInteger) @safe nothrow @nogc bigIntegerDel,
            scope void delegate(bool) @safe nothrow @nogc booleanDel, scope void delegate(double) @safe nothrow @nogc numberDel,
            scope void delegate(String_UTF8) @safe nothrow @nogc textDel, scope void delegate() @safe nothrow @nogc nullDel) scope @trusted {

        if(isNull) {
            if(nullDel !is null)
                nullDel();
            return;
        }

        final switch(this.node.type) {
        case Type.Null:
            if(nullDel !is null)
                nullDel();
            break;

        case Type.Array:
            if(arrayDel !is null)
                arrayDel(this.node.array);
            break;
        case Type.Object:
            if(objectDel !is null)
                objectDel(this.node.obj);
            break;
        case Type.BigInteger:
            if(bigIntegerDel !is null)
                bigIntegerDel(this.node.bigInteger);
            break;
        case Type.Boolean:
            if(booleanDel !is null)
                booleanDel(this.node.boolean);
            break;
        case Type.Number:
            if(numberDel !is null)
                numberDel(this.node.number);
            break;
        case Type.Text:
            if(textDel !is null)
                textDel(this.node.text);
            break;
        }
    }

    ///
    static JSONValue create(Type type) @trusted {
        RCAllocator allocator = globalAllocator();
        JSONValue ret;

        ret.node = allocator.make!JSONState(1, allocator, type);

        final switch(type) {
        case Type.Null:
            break;
        case Type.Array:
            ret.node.array = LinkedList!JSONValue(allocator);
            break;
        case Type.Object:
            ret.node.obj = HashMap!(String_UTF8, JSONValue).init;
            break;
        case Type.BigInteger:
            ret.node.bigInteger = DynamicBigInteger.init;
            break;
        case Type.Boolean:
            ret.node.boolean = false;
            break;
        case Type.Number:
            ret.node.number = double.nan;
            break;
        case Type.Text:
            ret.node.text = String_UTF8.init;
            break;
        }

        return ret;
    }

    ///
    ulong toHash() scope const {
        return cast(ulong)this.node;
    }

    ///
    bool opEquals(scope JSONValue other) scope const {
        return this.node is other.node;
    }

    ///
    int opCmp(scope JSONValue other) scope @trusted {
        import sidero.base.containers.utils : genericCompare;

        if(this.node is other.node)
            return 0;
        else if(this.isNull)
            return -1;
        else if(other.isNull)
            return 1;
        else if(this.node.type < other.node.type)
            return -1;
        else if(this.node.type > other.node.type)
            return 1;

        final switch(this.node.type) {
        case JSONValue.Type.Null:
            return 0;
        case JSONValue.Type.Array:
            return this.node.array.opCmp(other.node.array);
        case JSONValue.Type.Object:
            return this.node.obj.opCmp(other.node.obj);
        case JSONValue.Type.BigInteger:
            return this.node.bigInteger.opCmp(other.node.bigInteger);
        case JSONValue.Type.Boolean:
            return this.node.boolean.genericCompare(other.node.boolean);
        case JSONValue.Type.Number:
            return this.node.number.genericCompare(other.node.number);
        case JSONValue.Type.Text:
            return this.node.text.opCmp(other.node.text);
        }
    }

    ///
    String_UTF8 toString() @trusted {
        StringBuilder_UTF8 ret = StringBuilder_UTF8();
        toString(ret);
        return ret.asReadOnly;
    }

    ///
    void toString(Sink)(scope ref Sink sink) @trusted {
        if(isNull) {
            sink ~= "JSONValue@null";
            return;
        }

        sink.formattedWrite("JSONValue@{:p}(type={:s}, have comments={:s})", cast(void*)this.node, this.node.type,
                this.haveAttachedComments());
    }

    ///
    String_UTF8 toStringPretty(PrettyPrint pp) @trusted {
        StringBuilder_UTF8 ret = StringBuilder_UTF8();
        toStringPretty(ret, pp);
        return ret.asReadOnly;
    }

    ///
    void toStringPretty(Sink)(scope ref Sink sink, PrettyPrint pp) @trusted {
        pp.emitPrefix(sink);

        if(isNull) {
            sink ~= "JSONValue@null";
            return;
        }

        pp.depth++;

        if(this.node.type == Type.Null) {
            sink.formattedWrite("JSONValue@{:p}(type={:s}", cast(void*)this.node, this.node.type);
        } else {
            sink.formattedWrite("JSONValue@{:p}(type={:s} =>", cast(void*)this.node, this.node.type);

            this.match((LinkedList!JSONValue array) {
                sink ~= "\n";

                pp.startWithoutPrefix = false;
                array.toStringPretty(sink, pp);
            }, (HashMap!(String_UTF8, JSONValue) obj) {
                sink ~= "\n";

                pp.startWithoutPrefix = false;
                obj.toStringPretty(sink, pp);
            }, (DynamicBigInteger bigInteger) { sink.formattedWrite(" {:s}", bigInteger); }, (bool boolean) {
                sink ~= boolean ? " true" : " false";
            }, (double number) { sink.formattedWrite(" {:s}", number); }, (String_UTF8 text) {
                sink ~= "\n";

                pp.startWithoutPrefix = false;
                pp(sink, text);
            }, () => assert(0));
        }

        if(this.haveAttachedComments()) {
            sink ~= ", comments =>\n";

            pp.startWithoutPrefix = false;
            pp.depth++;
            this.attachedComments().toStringPretty(sink , pp);

            sink ~= ")";
            pp.depth--;
        } else
            sink ~= ", have comments=false)";

        pp.depth--;
    }

    ///
    enum Type {
        Null, ///
        Array, ///
        Object, ///
        BigInteger, ///
        Boolean, ///
        Number, ///
        Text ///
    }
}

///
unittest {
    import sidero.base.math.utils;

    JSONValue value = JSONValue.create(JSONValue.Type.Null);
    assert(!value.isNull);

    value.match((LinkedList!JSONValue) => assert(0), (HashMap!(String_UTF8, JSONValue)) => assert(0),
            (DynamicBigInteger) => assert(0), (bool) => assert(0), (double number) => assert(0), (String_UTF8) => assert(0), () {
    });

    value.attachedComments() ~= String_UTF8("Some comment goes here");
    assert(!value.isNull);

    value = true;
    value.match((LinkedList!JSONValue) => assert(0), (HashMap!(String_UTF8, JSONValue)) => assert(0),
            (DynamicBigInteger) => assert(0), (bool boolean) => assert(boolean), (double) => assert(0),
            (String_UTF8) => assert(0), () => assert(0));

    value = 42;
    value.match((LinkedList!JSONValue) => assert(0), (HashMap!(String_UTF8, JSONValue)) => assert(0),
            (DynamicBigInteger) => assert(0), (bool) => assert(0), (double number) => assert(isClose(number, 42)),
            (String_UTF8) => assert(0), () => assert(0));

    value = DynamicBigInteger.parse("95");
    value.match((LinkedList!JSONValue) => assert(0), (HashMap!(String_UTF8, JSONValue)) => assert(0),
            (DynamicBigInteger bigInteger) => assert(bigInteger == DynamicBigInteger.parse("95")), (bool) => assert(0),
            (double) => assert(0), (String_UTF8) => assert(0), () => assert(0));

    value = String_UTF8("Hi there!");
    value.match((LinkedList!JSONValue) => assert(0), (HashMap!(String_UTF8, JSONValue)) => assert(0),
            (DynamicBigInteger) => assert(0), (bool) => assert(0), (double) => assert(0),
            (String_UTF8 text) => assert(text == "Hi there!"), () => assert(0));

    {
        JSONValue temp = JSONValue.create(JSONValue.Type.Text);
        temp = String_UTF8("Hi there!");

        value = Slice!JSONValue(temp);
        value.match((LinkedList!JSONValue array) => assert(array == temp), (HashMap!(String_UTF8,
                JSONValue)) => assert(0), (DynamicBigInteger) => assert(0), (bool) => assert(0), (double) => assert(0),
                (String_UTF8) => assert(0), () => assert(0));
    }

    {
        String_UTF8 key = "akey";
        JSONValue temp = JSONValue.create(JSONValue.Type.Text);
        temp = String_UTF8("Hi there!");

        HashMap!(String_UTF8, JSONValue) kv;
        kv[key] = temp;

        value = kv;
        value.match((LinkedList!JSONValue) => assert(0), (HashMap!(String_UTF8, JSONValue) map) => assert(key in map),
                (DynamicBigInteger) => assert(0), (bool) => assert(0), (double) => assert(0), (String_UTF8) => assert(0), () => assert(0));

        import sidero.base.console;
        StringBuilder_UTF8 builder;
        PrettyPrint pp = PrettyPrint.defaults;

        pp(builder, value);

        writeln(builder);
    }

    assert(value.attachedComments() == [String_UTF8("Some comment goes here")]);
}

package(sidero.fileformats):

struct JSONState {
    int refCount;
    RCAllocator allocator;

    JSONValue.Type type;
    DynamicArray!String_UTF8 attachedComments;

    union {
        LinkedList!JSONValue array;
        HashMap!(String_UTF8, JSONValue) obj;
        DynamicBigInteger bigInteger;
        bool boolean;
        double number;
        String_UTF8 text;
    }

@safe nothrow @nogc:

     ~this() scope {
        this.cleanup;
    }

    void cleanup() scope @trusted {
        final switch(type) {
        case JSONValue.Type.Null:
            break;
        case JSONValue.Type.Array:
            this.array.destroy;
            break;
        case JSONValue.Type.Object:
            this.obj.destroy;
            break;
        case JSONValue.Type.BigInteger:
            this.bigInteger.destroy;
            break;
        case JSONValue.Type.Boolean:
        case JSONValue.Type.Number:
            break;
        case JSONValue.Type.Text:
            this.text.destroy;
            break;
        }
    }

    void rc(bool addRef) scope @trusted {
        if(addRef) {
            refCount++;
        } else if(refCount-- == 1) {
            // no longer known about
            RCAllocator allocator = this.allocator;
            allocator.dispose(&this);
        }
    }
}
