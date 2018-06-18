public class PokerHand implements Comparator<PokerHand> {
  /* http://stackoverflow.com/questions/30449488/comparison-method-violates-its-general-contract-everything-seems-ok */

  int indexOf(int pos);
  int charAt(int i);
  int countOccurrencesOf(int i);
  int lastIndexOf(int i);
  int id;
  int min123;
  int min12;
  int min13;
  int min23;


  public int pick(PokerHand o1, PokerHand o2, PokerHand o3) {
        assume (o1.id != o2.id);
        assume (o1.id != o3.id);
        assume (o2.id != o3.id);
        if (o1.id <= o2.id) {
          assume (o1.min12 == o1.id);
        } else {
          assume (o1.min12 == o2.id);
        }
        if (o1.id <= o3.id) {
          assume (o1.min13 == o1.id);
        } else {
          assume (o1.min13 == o3.id);
        }
        if (o2.id <= o3.id) {
          assume (o1.min23 == o2.id);
        } else {
          assume (o1.min23 == o3.id);
        }
        if (o1.id <= o1.min23) {
          assume (o1.min123 == o1.id);
        } else {
          assume (o1.min123 == o1.min23);
        }
        if ((o1.indexOf(4) != -1) || (o2.indexOf(4) != -1) || (o3.indexOf(4) != -1)) {  // Four of a kind
            if (o1.indexOf(4) == o2.indexOf(4)) {
                if (o2.indexOf(4) == o3.indexOf(4)) {
                  int i1 = 0;
                  while(i1 <= 12){
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4) && (o2.charAt(i1) != 0) && (o2.charAt(i1) != 4) &&
                        (o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o1.min123;
                    }
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4) && (o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o3.id;
                    }
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4) && (o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o2.id;
                    }
                    if ((o2.charAt(i1) != 0) && (o2.charAt(i1) != 4) && (o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o1.id;
                    }
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4)) {
                        return o1.min23;
                    }
                    if ((o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o1.min13;
                    }
                    if ((o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o1.min12;
                    }
                    i1++;
                  }
                  return o1.min123;
                }
                int i1 = 0;
                while(i1 <= 12){
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4) && (o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o1.min12;
                    }
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4)) {
                        return o2.id;
                    }
                    if ((o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o1.id;
                    }
                    i1++;
                }
                return o1.min12;
            }
            if (o2.indexOf(4) == o3.indexOf(4)) {
                int i1 = 0;
                while(i1 <= 12){
                    if ((o2.charAt(i1) != 0) && (o2.charAt(i1) != 4) && (o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o1.min23;
                    }
                    if ((o2.charAt(i1) != 0) && (o2.charAt(i1) != 4)) {
                        return o3.id;
                    }
                    if ((o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o2.id;
                    }
                    i1++;
                }
                return o1.min23;
            }
            if (o1.indexOf(4) == o3.indexOf(4)) {
                int i1 = 0;
                while(i1 <= 12){
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4) && (o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o1.min13;
                    }
                    if ((o1.charAt(i1) != 0) && (o1.charAt(i1) != 4)) {
                        return o3.id;
                    }
                    if ((o3.charAt(i1) != 0) && (o3.charAt(i1) != 4)) {
                        return o1.id;
                    }
                    i1++;
                }
                return o1.min13;
            }
            if (o1.indexOf(4) - o2.indexOf(4) > 0) {
              if (o1.indexOf(4) - o3.indexOf(4) > 0) {
                return o1.id;
              }
              return o3.id;
            } else if (o2.indexOf(4) - o3.indexOf(4) > 0) {
              return o2.id;
            }
            return o3.id;
        }
        return o1.min123;
    }
}
