
## Graphe


### Compilation

```
ocamlopt deQueue.ml tools.ml iO.ml graph.ml -o main main.ml
```

### Usage


```
./main courbe N
```

```
./main graphe N
```

### Gnuplot

```
dot -Tps -o graph.ps graph.dot
open graph.ps
```

