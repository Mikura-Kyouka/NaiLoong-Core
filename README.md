## About
This is the respository for NSCSCC 2025. 

## Features
### Completed
- None for now.

### WIP
- Base Core
  - OOE
    - Basic instructions
    - CSR (later)
    - ...And more
  - Exception support
  - Tomasulo Algorithm
  - Difftest
  - ...And more
- Profiler
- Caches (L1_i, L1_d)
- Branch Prediction
- Other functions

## Code Formatting Criteria
While coding with C/C++, consider styling as [GNU format](https://www.gnu.org/prep/standards/html_node/Formatting.html). It is also recommended to apply that style to other languages if could.

## Commits related
While committing, consider formatting in the style below:
```
<verb>(<scope>): <what you did>
```
If you the scope is global, simply use a "*".

Examples:
```
feat(*): add xxxxx
chore(Makefile): modify info
fix(build): changed xxx build process
... And more
```

Every commit should be signatured with GnuPG (aka GPG key), so that all commits will be verified.

It is discouraged that directly commit into the main branch, use Pull Request instead. 