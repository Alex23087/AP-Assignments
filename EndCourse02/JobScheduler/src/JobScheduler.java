import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.function.BinaryOperator;
import java.util.function.Supplier;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class JobScheduler<K,V> {
    public static void main(String[] args) {
        JobScheduler js = new JobScheduler() {
            @Override
            Stream<AJob> emit() {
                return null;
            }

            @Override
            void output(Stream stream) {

            }
        };

        js.output(js.collect(js.compute(js.emit())));
    }

    private Stream<Pair<K,V>> compute(Stream<AJob<K,V>> stream){
        return stream.flatMap(AJob::execute);
    }
    private Stream<Pair<K, List<V>>> collect(Stream<Pair<K,V>> stream){
        return stream.collect(
//                HashMap<K, List<V>>::new,
//                (kListHashMap, kvPair) -> {
//                    if(!kListHashMap.containsKey(kvPair.getKey())){
//                        kListHashMap.put(kvPair.getKey(), new ArrayList<>());
//                    }
//                    kListHashMap.get(kvPair.getKey()).add(kvPair.getValue());
//                },
//                ((kListHashMap, kListHashMap2) -> {
//                    kListHashMap.merge()
//                }
                Collectors.toMap(
                        p -> p.getKey(),
                        p -> {
                            ArrayList<V> al = new ArrayList<>();
                            al.add(p.getValue());
                            return al;
                        },
                        (value1, value2) -> {
                            value1.addAll(value2);
                            return value1;
                        }
                )
        ).entrySet().stream().map(e -> new Pair<>(e.getKey(), e.getValue()));
    }

    abstract Stream<AJob<K,V>> emit();
    abstract void output(Stream<Pair<K, List<V>>> stream);
}
