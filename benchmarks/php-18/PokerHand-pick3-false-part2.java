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


  public int compare(PokerHand o1, PokerHand o2, PokerHand o3) {
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
        int tripleCount1 = o1.countOccurrencesOf(3);
        int tripleCount2 = o2.countOccurrencesOf(3);
        int tripleCount3 = o3.countOccurrencesOf(3);

        if ((tripleCount1 > 1) || ((tripleCount1 == 1) && (o1.indexOf(2) != -1))) {      // c1 Full house
            if ((tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {  // c2 Full house too
                if ((tripleCount3 > 1) || ((tripleCount3 == 1) && (o3.indexOf(2) != -1))) {  // c3 Full house too
                   int higherTriple = o1.lastIndexOf(3);
                   if ((higherTriple == o2.lastIndexOf(3)) && (higherTriple == o3.lastIndexOf(3))) {
                      int i2=0;
                      while(i2 <= 12){
                          if ((i2 != higherTriple) && (((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)) &&
                             ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3)))) {
                              return o1.min123;
                          }
                          if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) {
                              return o1.id;                                                
                          }
                          if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) {
                              return o2.id;                                                
                          }
                          if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) {
                              return o3.id;                                                
                          }
                          if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3))) {
                              return o1.min23;
                          }
                          if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) {
                              return o1.min13;
                          }
                          if ((i2 != higherTriple) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) {
                              return o1.min12;
                          }
                          i2++;
                      }
                      return o1.min123;
                  }
                  if (higherTriple == o2.lastIndexOf(3)) {
                      int i2=0;
                      while(i2 <= 12){
                          if ((i2 != higherTriple) && (((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)))){
                              return o1.min12;
                          }
                          if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) {
                              return o1.id;                                                
                          }
                          if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3))) {
                              return o2.id;                                                
                          }
                          i2++;
                      }
                      return o1.min12;
                  }
                  if (higherTriple == o3.lastIndexOf(3)) {
                      int i2=0;
                      while(i2 <= 12){
                          if ((i2 != higherTriple) && (((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3)))){
                              return o1.min13;
                          }
                          if ((i2 != higherTriple) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) {
                              return o1.id;                                                
                          }
                          if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3))) {
                              return o3.id;                                                
                          }
                          i2++;
                      }
                      return o1.min13;
                  }
                  higherTriple = o2.lastIndexOf(3);
                  if (higherTriple == o3.lastIndexOf(3)) {
                      int i2=0;
                      while(i2 <= 12){
                          if ((i2 != higherTriple) && (((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3)))){
                              return o1.min23;
                          }
                          if ((i2 != higherTriple) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) {
                              return o2.id;                                                
                          }
                          if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) {
                              return o3.id;                                                
                          }
                          i2++;
                      }
                      return o1.min23;
                  }
                  if ((o1.lastIndexOf(3) - o2.lastIndexOf(3)) > 0) {
                    if ((o1.lastIndexOf(3) - o3.lastIndexOf(3)) > 0) {
                      return o1.id;
                      }
                    return o3.id;
                  }
                  if ((o2.lastIndexOf(3) - o3.lastIndexOf(3)) > 0) {
                    return o2.id;
                  }
                  return o3.id;
                }
                int higherTriple = o1.lastIndexOf(3);
                if (higherTriple == o2.lastIndexOf(3)) {
                  int i2=0;
                  while(i2 <= 12){
                      if ((i2 != higherTriple) && (((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)))) {
                          return o1.min12;
                      }
                      if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3))) { // only c1 Full house
                          return o2.id;                                                
                      }
                      if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) { // only c2 Full house
                          return o1.id;
                      }
                  }
                  return o1.min12;
                }
                if ((higherTriple - o2.lastIndexOf(3)) > 0) {
                  return o1.id;
                } else {
                  return o2.id;
                }
            }
            if ((tripleCount3 > 1) || ((tripleCount3 == 1) && (o3.indexOf(2) != -1))) {  // c3 Full house too
                int higherTriple = o1.lastIndexOf(3);
                if (higherTriple == o3.lastIndexOf(3)) {
                  int i2=0;
                  while(i2 <= 12){
                      if ((i2 != higherTriple) && (((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3)) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3)))) {
                          return o1.min13;
                      }
                      if ((i2 != higherTriple) && ((o1.charAt(i2) == 2) || (o1.charAt(i2) == 3))) { // only c1 Full house
                          return o3.id;                                                
                      }
                      if ((i2 != higherTriple) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) { // only c3 Full house
                          return o1.id;
                      }
                  }
                }
                if ((higherTriple - o3.lastIndexOf(3)) > 0) {
                  return o1.id;
                } else {
                  return o3.id;
                }
            }
            return o1.id;
        }
        if ((tripleCount2 > 1) || ((tripleCount2 == 1) && (o2.indexOf(2) != -1))) {       // c2 Full house
            if ((tripleCount3 > 1) || ((tripleCount3 == 1) && (o3.indexOf(2) != -1))) {  // c3 Full house too
                int higherTriple = o2.lastIndexOf(3);
                if (higherTriple == o3.lastIndexOf(3)) {
                  int i2=0;
                  while(i2 <= 12){
                      if ((i2 != higherTriple) && (((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3)) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3)))) {
                          return o1.min23;
                      }
                      if ((i2 != higherTriple) && ((o2.charAt(i2) == 2) || (o2.charAt(i2) == 3))) { // only c2 Full house
                          return o3.id;                                                
                      }
                      if ((i2 != higherTriple) && ((o3.charAt(i2) == 2) || (o3.charAt(i2) == 3))) { // only c3 Full house
                          return o2.id;
                      }
                  }
                  return o1.min23;
                }
                if ((higherTriple - o3.lastIndexOf(3)) > 0) {
                  return o2.id;
                } else {
                  return o3.id;
                }
            }
            return o2.id;
        }
        if ((tripleCount3 > 1) || ((tripleCount3 == 1) && (o3.indexOf(2) != -1))) {
            return o3.id;
        }
        return o1.min123;
    }
}
