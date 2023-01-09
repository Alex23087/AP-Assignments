package it.unipi.puzzle;

import java.util.List;

@FunctionalInterface
public interface RestartListener {
    void restart(List<Integer> labels);
}
