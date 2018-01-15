public class PokerHand implements Comparator<PokerHand> {
  /* http://stackoverflow.com/questions/30449488/comparison-method-violates-its-general-contract-everything-seems-ok */

  int indexOf(int pos);
  int charAt(int i);
  int countOccurrencesOf(int i);
  int lastIndexOf(int i);
  int id;
  int smallid;

  public int compare(PokerHand o1, PokerHand o2) {
        assume (o1.id != o2.id);
        if (o1.id < o2.id) {
          assume (o1.smallid == o1.id);
        } else {
          assume (o1.smallid == o2.id);
        }
        if ((o1.indexOf(4) != -1) || (o2.indexOf(4) != -1)) {  // Four of a kind
            if (o1.indexOf(4) == o2.indexOf(4)) {
                int i1 = 0;
                while(i1 <= 12){
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4) && (o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o1.smallid;
                    }
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4)) {
                        return o2.id;
                    }
                    if ((o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o1.id;
                    }
                    i1++;
                }
                return o1.smallid;
            }
            if (o1.indexOf(4) - o2.indexOf(4) > 0) {
              return o1.id;
            } else {
              return o2.id;
            }
        }
        int tripleCount1 = o1.countOccurrencesOf(3);
        int tripleCount2 = o2.countOccurrencesOf(3);

        if ((tripleCount1 > 1) || ((tripleCount1 == 1) && (o1.indexOf(2) != -1))) {      // c1 Full house
            if ((tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {  // c2 Full house too
                int higherTriple = o1.lastIndexOf(3);
                if (higherTriple == o2.lastIndexOf(3)) {
                    int i2=0;
                    while(i2 <= 12){
                        if ((i2 != higherTriple) && (((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)))) {
                            return o1.smallid;
                        }
                        if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3))) { // only c1 Full house
                            return o2.id;                                                
                        }
                        if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) { // only c2 Full house
                            return o1.id;
                        }
                    }
                    return o1.smallid;
                }
                if ((higherTriple - o2.lastIndexOf(3)) > 0) {
                  return o1.id;
                } else {
                  return o2.id;
                }
            }
            return o1.id;
        }
        if ((tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {
            return o2.id;
        }
        return o1.smallid;
    }
}
