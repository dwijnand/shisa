package shisa;

import java.nio.file.Path;

public interface Invoke {
    String id();
    String cmd();
    Runner mkRunner();
    default CompileResult compile1(Path src) { return mkRunner().compile1(src); }
}
