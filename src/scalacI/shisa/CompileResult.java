package shisa;

import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;

public final class CompileResult {
    public final List<Msg> msgs;

    public CompileResult(List<Msg> msgs) {
        this.msgs = msgs;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CompileResult)) return false;
        CompileResult that = (CompileResult) o;
        return msgs.equals(that.msgs);
    }

    public int hashCode() { return Objects.hash(msgs); }

    public String toString() {
        return new StringJoiner(", ", CompileResult.class.getSimpleName() + "[", "]")
                .add("msgs=" + msgs)
                .toString();
    }
}
