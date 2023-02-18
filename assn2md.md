# Question 1

### Applicative Order (Inner first)

$
(L \ xyz\  |\  x\ z\ (y\ y\ z)) (Lx|x) (Lx|xy) a \\
→ (Lx|x) a ((Lx|xy) (Lx|xy) a) \\
→ (Lx|x) a ((Lx|xy) y a) \\
→ (Lx|x) a (y y a) \\
→ a (y y a) \\
$
&nbsp;

Normal Order (Outer first)

$
(Lxyz | xz(yyz)) (Lx|x) (Lx|xy) a \\
→ (Lx|x) a ((Lx|xy) (Lx|xy) a) \\
→ a ((Lx|xy)(Lx|xy) a) \\
→ a ((Lx|xy) y a) \\
→ a (y y a) \\
$
&nbsp;
# Question 2

