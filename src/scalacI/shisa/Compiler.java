package shisa;

import java.nio.file.Path;

public interface Compiler {
    String id();
    String cmd();
    CompileResult compile1(Path src);
}
