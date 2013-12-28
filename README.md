
## Graphe


### Compilation

```
ocamlc deQueue.ml tools.ml iO.ml graph.ml dialogue.ml -o main main.ml
```

### Usage


```
./main -courbe N
```

```
./main -graph
```

### Gnuplot

```
dot -Tps -o graph.ps graph.dot
open graph.ps
```

