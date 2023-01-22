import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class AnagramCounter extends JobScheduler<String, Integer>{

    @Override
    Stream<AJob<String, Integer>> emit() {
        System.out.println("Absolute path of the folder to scan: ");
        Scanner scanner = new Scanner(System.in);
        String pathname = scanner.nextLine();
        try {
            return Files.list(Paths.get(pathname)).map(Path::toString).filter(s -> s.matches("^.*\\.txt$")).map(AnagramJob::new);
        } catch (IOException e) {
            System.err.println("Invalid path");
            return Stream.of();
        }
    }

    @Override
    void output(Stream<Pair<String, List<Integer>>> stream) {
        try {
            Files.write(Paths.get("count_anagrams.txt"),
                    stream.map(p -> new Pair<>(p.getKey(), p.getValue().stream().reduce(Integer::sum).orElse(0)))
                            .map(p -> String.format("<%s> - <%d>", p.getKey(), p.getValue()))
                            .toList()
            );
        } catch (IOException e) {
            System.err.println("Unable to write to file");
        }
    }

    private static class AnagramJob extends AJob<String, Integer>{
        private final String filename;
        AnagramJob(String filename){
            this.filename = filename;
        }

        @Override
        public Stream<Pair<String, Integer>> execute() {
            try {
                return Arrays.stream(Files.readString(Paths.get(filename)).split(" "))
                        .filter(s -> s.matches("^[a-zA-Z]{4,}$"))
                        .map(String::toLowerCase)
                        .map(s -> {
                            char[] chararray = s.toCharArray();
                            Arrays.sort(chararray);
                            return new String(chararray);
                        })
                        .collect(Collectors.toMap(
                                String::toString,
                                (s) -> 1,
                                Integer::sum
                        )).entrySet().stream().map(e -> new Pair<>(e.getKey(), e.getValue()));
            } catch (IOException e) {
                return Stream.of();
            }
        }
    }
}
