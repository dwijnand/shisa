package shisa;

import java.util.List;
import java.util.Objects;
import java.util.StringJoiner;

public final class CompileResult {
    public final boolean hasErrors;
    public final List<String> lines;

    public CompileResult(boolean hasErrors, List<String> lines) {
        this.hasErrors = hasErrors;
        this.lines = lines;
    }

    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof CompileResult)) return false;
        CompileResult that = (CompileResult) o;
        return hasErrors == that.hasErrors && lines.equals(that.lines);
    }

    public int hashCode() {
        return Objects.hash(hasErrors, lines);
    }

    public String toString() {
        return new StringJoiner(", ", CompileResult.class.getSimpleName() + "[", "]")
                .add("hasErrors=" + hasErrors)
                .add("lines=" + lines)
                .toString();
    }
}

// scalac2: ERROR WARNING INFO
// scalac3: Error FeatureWarning DeprecationWarning UncheckedWarning MigrationWarning Warning Info
// AbstractFile { name: String, path: String, jfile: Optional[File] }
// SourceFile <: AbstractFile { content: Array[Char] }
// SourcePosition {
//          source: SourceFile,
//     lineContent: String,
//     point,      line,      column: Int,
//     start, startLine, startColumn: Int,
//       end,   endLine,   endColumn: Int,
// }
