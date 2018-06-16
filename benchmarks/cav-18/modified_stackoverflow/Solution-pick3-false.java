/*
 * Based on http://codedbot.com/questions/402185/java-comparator-violates-general-contract 
 * 
 */
public class SolutionComparator implements Comparator<SolutionComparator> {
    int getValue;
    int solutionCost;
    int id;
    int min12;
    int min13;
    int min23;
    int min123;

    @Override
    public int pick(SolutionComparator o1, SolutionComparator o2, SolutionComparator o3) {
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
        int v1 = o1.getValue;
        int v2 = o2.getValue;
        int v3 = o3.getValue;
        int target = nondet(0);
        // Math.abs(target-v1)
        int comp1 = target-v1;
        int abscomp1 = 0;
        if (comp1 >= 0) 
          abscomp1 = comp1;
        else
          abscomp1 = -comp1;
       
        // Math.abs(target-v2)
        int comp2 = target-v2;
        int abscomp2 = 0;
        if (comp2 >= 0) 
          abscomp2 = comp2;
        else
          abscomp2 = -comp2;

        // Math.abs(target-v3)
        int comp3 = target-v3;
        int abscomp3 = 0;
        if (comp3 >= 0) 
          abscomp3 = comp3;
        else
          abscomp3 = -comp3;

        if ((v1 == -1) && (v2 == -1) && (v3 == -1))
            return o1.min123;
        else if ((v1 == -1) && (v2 == -1))
            return o3.id;
        else if ((v1 == -1) && (v3 == -1))
            return o2.id;
        else if ((v2 == -1) && (v3 == -1))
            return o1.id;
        else if (v1 == -1) {
            if (abscomp2 == abscomp3){ // (Math.abs(target-v2) == Math.abs(target-v3))
                //return (int)Math.signum(solutionCost(o2) - solutionCost(o3));
                int comp = o2.solutionCost - o3.solutionCost;
                if (comp > 0)
                return o3.id;
                else if (comp < 0)
                return o2.id;
                else 
                  return o1.min23;
            }
            else {
                //return (int)Math.signum(Math.abs(target-v2) - Math.abs(target-v3));
                int comp4 = abscomp2 - abscomp3;
                if (comp4 > 0)
                return o3.id;
                else if (comp4 < 0)
                return o2.id;
                else
                return o1.min23; 
           }
        }
        else if (v2 == -1) {
            if (abscomp1 == abscomp3){ // (Math.abs(target-v1) == Math.abs(target-v3))
                //return (int)Math.signum(solutionCost(o1) - solutionCost(o3));
                int comp = o1.solutionCost - o3.solutionCost;
                if (comp > 0)
                return o3.id;
                else if (comp < 0)
                return o1.id;
                else 
                  return o1.min13;
            }
            else {
                //return (int)Math.signum(Math.abs(target-v1) - Math.abs(target-v3));
                int comp4 = abscomp1 - abscomp3;
                if (comp4 > 0)
                return o3.id;
                else if (comp4 < 0)
                return o1.id;
                else
                return o1.min13; 
           }
        }
        else if (v3 == -1) {
            if (abscomp1 == abscomp2){ // (Math.abs(target-v1) == Math.abs(target-v2))
                //return (int)Math.signum(solutionCost(o1) - solutionCost(o2));
                int comp = o1.solutionCost - o2.solutionCost;
                if (comp > 0)
                return o2.id;
                else if (comp < 0)
                return o1.id;
                else 
                  return o1.min12;
            }
            else {
                //return (int)Math.signum(Math.abs(target-v1) - Math.abs(target-v2));
                int comp4 = abscomp1 - abscomp2;
                if (comp4 > 0)
                return o2.id;
                else if (comp4 < 0)
                return o1.id;
                else
                return o1.min12; 
            }
        }
        else if ((abscomp1 == abscomp2) && (abscomp2 == abscomp3)){
            int comp12 = o1.solutionCost - o2.solutionCost;
            int comp13 = o1.solutionCost - o3.solutionCost;
            int comp23 = o2.solutionCost - o3.solutionCost;
            if (comp12 > 0) {
                if (comp23 > 0) {
                    return o3.id;
                }
                else if (comp23 < 0) {
                    return o2.id;
                }
                return o1.min23;
            }
            else if (comp12 < 0) {
                if (comp13 > 0) {
                    return o3.id;
                }
                else if (comp13 < 0) {
                    return o1.id;
                }
                return o1.min13;
            }
            else {
                if (comp13 > 0) {
                    return o3.id;
                }
                else if (comp13 < 0) {
                    return o1.min12;
                }
                return o1.min123;
            }
        }
        else if (abscomp1 == abscomp2){ // (Math.abs(target-v1) == Math.abs(target-v2))
            //return (int)Math.signum(solutionCost(o1) - solutionCost(o2));
            int comp13 = abscomp1 - abscomp3;
            if (comp13 > 0)
              return o3.id;
            int comp = o1.solutionCost - o2.solutionCost;
            if (comp > 0)
            return o2.id;
            else if (comp < 0)
            return o1.id;
            else 
              return o1.min12;
        }
        else if (abscomp1 == abscomp3){ // (Math.abs(target-v1) == Math.abs(target-v3))
            //return (int)Math.signum(solutionCost(o1) - solutionCost(o3));
            int comp12 = abscomp1 - abscomp2;
            if (comp12 > 0)
              return o2.id;
            int comp = o1.solutionCost - o3.solutionCost;
            if (comp > 0)
            return o3.id;
            else if (comp < 0)
            return o1.id;
            else 
              return o1.min13;
        }
        else if (abscomp2 == abscomp3){ // (Math.abs(target-v2) == Math.abs(target-v3))
            //return (int)Math.signum(solutionCost(o2) - solutionCost(o3));
            int comp21 = abscomp2 - abscomp1;
            if (comp21 > 0)
              return o1.id;
            int comp = o2.solutionCost - o3.solutionCost;
            if (comp > 0)
            return o3.id;
            else if (comp < 0)
            return o2.id;
            else 
              return o1.min23;
        }
        else {
           //return (int)Math.signum(Math.abs(target-v1) - Math.abs(target-v2));
           int comp12 = abscomp1 - abscomp2;
           int comp13 = abscomp1 - abscomp3;
           int comp23 = abscomp2 - abscomp3;
           if (comp12 > 0) {
               if (comp23 > 0)
                   return o3.id;
               else if (comp23 < 0)
                   return o2.id;
               else
                   return o1.min23;
           }
           else if (comp12 < 0) {
               if (comp13 > 0)
                   return o3.id;
               else if (comp13 < 0)
                   return o1.id;
               else
                   return o1.min13;
           } 
           else if (comp23 > 0)
               return o3.id;
           else if (comp23 < 0)
               return o1.min13;
           else
               return o1.min123;
        }
    }
}
