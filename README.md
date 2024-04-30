The unification algorithm that computes the most general unifier for equations **E** = {l1 = r1, ..., ln = rn}:

- If **n** = 0, return the identity substitution, i.e., **{}**.
- Otherwise, consider the equation l1 = r1.

  **--** If the equation already holds, delete it from **E** and recurse.

  **--** If l1 = f(t1, ..., tm) and r1 = f′(t′1, ..., t′m′),
    * if f ≠ f′ or m ≠ m′, then **E** has no solution;
    * otherwise, replace l1 = r1 with the equations t1 = t′1, ..., tm = t′m′ and recurse.

  **--** Otherwise, if l1 and r1 are not syntactically the same, and at least one of l1, r1 is a variable. Without loss of generality, suppose l1 is a variable **x**.
    * If **x** occurs in **r1**, then **E** has no solution (e.g., x = f(x)).
    * Otherwise, recurse on {l2 = r2, ..., ln = rn}{x → r1} (i.e., apply {x → r1} to the rest of the equations).
      - If the subproblem has a solution σ, then {x → r1}σ is a solution to **E**.
      - Otherwise, **E** has no solution.
