The unification algorithm that computes the most general unifier
for equations \emph{E} = \emph{\{l}1 = \emph{r}1, . . . , \emph{ln} =
\emph{rn\}}:\\

• If \emph{n} = 0, return the identity substitution, i.e. \emph{\{\}}.\\
• Otherwise, consider the equation \emph{l}1 = \emph{r}1.

\textbf{--} If the equation already holds, delete it from \emph{E} and
recurse.

\textbf{--} If \emph{l}1 = \emph{f} (\emph{t}1, . . . , \emph{tm}) and
\emph{r}1 = \emph{f′}(\emph{t′}1, . . . , \emph{t′m′}),\\
* if \emph{f ̸}= \emph{f ′} or \emph{m ̸}= \emph{m′}, then \emph{E} has
no solution;\\
* otherwise, replace \emph{l}1= \emph{r}1with the equations \emph{t}1=
\emph{t′}1, . . . , \emph{tm} = \emph{t′m′} and recurse.

\textbf{--} Otherwise, \emph{l}1 and \emph{r}1 are not syntactically the
same, and at least one of \emph{l}1, \emph{r}1 is a variable. Without
loss of generality, suppose \emph{l}1 is a variable \emph{x}.

* If \emph{x} occurs in \emph{r}1, then \emph{E} has no solution (e.g.
\emph{x} = \emph{f} (\emph{x})).

* Otherwise, recurse on \emph{\{l}2= \emph{r}2, . . . , \emph{ln} =
\emph{rn\}\{x �→ r}1\emph{\}} (i.e. apply \emph{\{x �→ r}1\emph{\}} to
the rest of the equations).

· If the subproblem has a solution \emph{σ}, then \emph{\{x �→
r}1\emph{\}σ} is a solution to \emph{E}.3· Otherwise, \emph{E} has no
solution.
