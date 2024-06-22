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
