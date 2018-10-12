import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

public class MSHint implements Assignment1 {
    
    public long minesweep (File inputfile) {
        String filename = inputfile.getPath();

        ArrayList<String> fieldDimensions = new ArrayList<>();

        ArrayList<String>[] fields = new ArrayList<String>;

        try (Stream<String> inputStream = Files.lines(Paths.get(filename))) {
            
            fieldDimensions = inputStream.filter(line -> !(line.startsWith(".") || line.startsWith("*")))
                                         .collect(Collectors.toList());
            
            mines = inputStream.filter(line -> (line.startsWith(".") || line.startsWith("*")))
                               .collect(Collectors.toList());

            for(String field : fields) {
                int width = Integer.parseInt(field.split(" ")[0]);
                int height = Integer.parseInt(field.split(" ")[1]);

            }

        }
    }

    static void generateArrays(String line) {
        
    }

}
