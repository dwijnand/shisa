package shisa;

import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;

public final class CompileResult {
    public final int exitCode;
    public final List<String> lines;

    public CompileResult(int exitCode, List<String> lines) {
        this.exitCode = exitCode;
        this.lines = lines;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CompileResult)) return false;
        CompileResult that = (CompileResult) o;
        return exitCode == that.exitCode &&
                lines.equals(that.lines);
    }

    public int hashCode() {
        return Objects.hash(exitCode, lines);
    }

    public String toString() {
        return new StringJoiner(", ", CompileResult.class.getSimpleName() + "[", "]")
                .add("exitCode=" + exitCode)
                .add("lines=" + lines)
                .toString();
    }
}
