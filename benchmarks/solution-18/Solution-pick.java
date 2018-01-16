/*
 * Based on http://codedbot.com/questions/402185/java-comparator-violates-general-contract 
 * 
 */
public class SolutionComparator implements Comparator<SolutionComparator> {
    int getValue;
    int solutionCost;
    int id;
    int min12;

    @Override
    public int compare(SolutionComparator o1, SolutionComparator o2) {
        assume (o1.id != o2.id);
        if (o1.id <= o2.id) {
          assume (o1.min12 == o1.id);
        } else {
          assume (o1.min12 == o2.id);
        }
        int v1 = o1.getValue;
        int v2 = o2.getValue;
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

        if ((v1 == -1) && (v2 == -1))
            return o1.min12;
        else if (v1 == -1)
            return o2.id;
        else if (v2 == -1)
            return o1.id;
        else if (abscomp1 == abscomp2){ // (Math.abs(target-v1) == Math.abs(target-v2))
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
            int comp3 = abscomp1 - abscomp2;
            if (comp3 > 0)
              return o2.id;
            else if (comp3 < 0)
              return o1.id;
            else
              return o1.min12; 
        }
    }
}
