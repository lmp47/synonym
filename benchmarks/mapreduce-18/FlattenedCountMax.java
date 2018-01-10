// Example based on CountMaxHadoop.java
// We want to check the Reducer for commutativity and associativity
public class Pair {
  int data (int i);
  int dataLength;
  int min;
  int id;
  int fst;
  int snd;

  public void equals(Pair o1, Pair o2) {
    assume(o1.min == o2.min);
    assume(o1.id != o2.id);
    if (o1.id < o2.id) {
      // map o1
      assume (o1.dataLength > 0);
      int i = 0;
      int first = o1.data(0);
      int second = 0;
      while (i < o1.dataLength) {
        if (first < o1.data(i)) {
          first = o1.data(i);
          second = 1;
        }
        else if (first == o1.data(i)) {
          second++;
        }
        i++;
      }
      assume (o1.fst == first);
      assume (o1.snd == second);
      // map o2
      assume (o2.dataLength > 0);
      int i = 0;
      int first = o2.data(0);
      int second = 0;
      while (i < o2.dataLength) {
        if (first < o2.data(i)) {
          first = o2.data(i);
          second = 1;
        }
        else if (first == o2.data(i)) {
          second++;
        }
        i++;
      }
      assume (o2.fst == first);
      assume (o2.snd == second);
    } else {
      // map o2
      assume (o2.dataLength > 0);
      int i = 0;
      int first = o2.data(0);
      int second = 0;
      while (i < o2.dataLength) {
        if (first < o2.data(i)) {
          first = o2.data(i);
          second = 1;
        }
        else if (first == o2.data(i)) {
          second++;
        }
        i++;
      }
      assume (o2.fst == first);
      assume (o2.snd == second);
      // map o1
      assume (o1.dataLength > 0);
      int i = 0;
      int first = o1.data(0);
      int second = 0;
      while (i < o1.dataLength) {
        if (first < o1.data(i)) {
          first = o1.data(i);
          second = 1;
        }
        else if (first == o1.data(i)) {
          second++;
        }
        i++;
      }
      assume (o1.fst == first);
      assume (o1.snd == second);
    }
    // reduce
    int first = o1.min;
    int second = 0;
    if (first < o1.fst) {
      first = o1.fst;
      second = o1.snd;
    }
    else if (first == o1.fst) {
      second += o1.snd;
    }
    if (first < o2.fst) {
      first = o2.fst;
      second = o2.snd;
    }
    else if (first == o2.fst) {
      second += o2.snd;
    }
    return second;
  }
}
