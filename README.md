I've worked on languages professionally for many years, now. I've always had to worry about pesky things like user adoption, familiarity and similarity to other languages, and proprietary compilation targets.

This is what happens when I can do whatever I want -- `petr` seeks to be a low-level performant general purpose programming languages, with a sophisticated type and side-effects-tracking system. And weird syntax! 

```
                  __         
    ____   ___   / /_   _____
   / __ \ / _ \ / __/  / ___/
  / /_/ //  __// /_   / /    
 / .___/ \___/ \__/  /_/     
/_/                         

```
**P**rogrammatic **E**ffects **Tr**acking


```
petr-ast                                                                                 
   │                                                                                     
   │                                                                                     
   ▼                                                                    ┌───►petr-codegen
petr-parse────►petr-bind────►petr-resolve────►petr-typecheck───► petr-ir│                
   │                                                                    ├───►petr-vm     
   │                                                                    │                 
   ▼                                                                    │                 
petr-fmt───────────────────────┬────────────────────────────────────────┘                                                      
                               │                                                         
                               │                                                         
                               │                                                         
                               ▼                                                         
petr-manifest────►petr-pkg────►pete                                                      
```
