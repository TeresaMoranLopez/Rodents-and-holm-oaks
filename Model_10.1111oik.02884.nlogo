breed [ mice mouse]
breed [na-mice na-mouse]
breed [na-mice-s na-mouse-s]
breed [acorns acorn]
globals [
         ;; GLOBAL VARIABLES RELATED TO FRAGMENTATION PROCESS
         fragmentation ; user defined it is the proportion of habitat subjected to forest fragmentation
         lots-half-width ; user defined. Croplots are assumed to have a rectangular size of area 8*(half-width^2). 
         ; In our base model croplots are assumed to have 30 meters width and 60 m length (1800 m2).
         half-width-normal; list with random numbers following a normal distribution -> lots-half-width and S.D. 2
         real-half-width-list; only those with width > 10 m. 
         ; In our model croplots of less than 800 m^2 are not allowed.
         n-lots ; Number of croplots to be created, it depends on the proportion of fragmentation and on croplots width.
         agregation.index ; fragmentation can occur in three different types of patterns: segregated, agregated or random. 
         ;this fragmentation pattern depends on the agregation.indeex which can be 0.1, 1 or 0.5 respectively. 
         ; In our base model agregation index is set to 1
          ;;;; GLOBAL VARIABLES RELATED TO FOREST TRAITS      
         number-stems ; user defined. It is the number of stems the world could potentially have if all the habitat was forest. 
         ;; because our world is of 5.76 ha, number of stems = stems/ha *5.76
         max.crop ; user defined. In our base model it is fixed to 10
         normal-radio ; user defined. It is a list in which values sampled from a normal distribution of mean R (radius) and a SD
         ;; In our base model it is fixed to 2.10, 0.66 (SD). From our field data
         radio-list ; it filters normal-radio to values > 1 m
         shrub; user defined, it is the proportion of understory with shrub cover.
         open.land.cover           
         ;;;GLOBAL VARIABLES RELATED TO CROP AND INTRASP-COMPETITION
         crop-list ; to know how area the characteristics of crop
         crop.average- 
         competitors-list ;  it is a list in which all active mice write their variable number-compeititors
         comp.average
         intra-sp-competition-list; it is a list in which all active mice write their perceived intraspecific competition for acorns  
         intrasp.comp.average        
         ;;;GLOBAL VARIABLES RELATED TO FRAGMENTATION DESCRIPTORS
         ;real habitat availability (since there are some geometrical restrictions to forest fragmentation real-fragmentation may differ to prior fragmentaiton)
         real-habitat-availability
         ; ;croplots output
         real-number-of-croplots ; to calculate the real number of croplots finally created
         crop-half-width-landscape ; a list with all croplots width
         real-half-width-mean ;calculate what is the area of croplots in reality since there are some restrictions (distance of croplots seeds > 10 m; aggregation index)
         ;; woodlots output
         number-of-woodlots ;variable related to  woodlots identification as well as the calculation of their area and core to edge ratio 
         area-list ; list with all woodlots area to calculate the mean woodlot area in landscape
         mean.area ; mean landscape area
         ;; core to edge-ratios
         core.edge-list ; list of core to edge area to calculat the mean in landscape
         mean.core.edge ; core to edge area in landscape
         max.area ; biggest woodlot area
         max.core.edge; core.edge of biggest woodlot
         ;;;;
         h.av.study
         core.edge.study
         ;;;;;GLOBAL VARIABLES MICE RELATED
         ;Active mice
         number-mice-prior; SLIDER. In our base model it is fixed to 10 which is the maximum number of mice per ha
         number-mice ; to calculate the real number of mice the forest can support which depends on the amount of shrub and canopy cover.
         ;Not-active-mice
         na-mice-buffer ; not active mice in the buffer area
         na-mice-study ; not active mice in the study area (important when stem density is very low but shrub encroachment occurs)
         ;;;GLOBAL VARIABLES RELATED WITH THE DISPERSAL PROCESS
         prob.pred; probability of predating an acorn in the firts meter it is fixed to 0.20
         ;output of mouse perception
         active-mouse ; very important when you hace more than one mice. If you don't ask 
         ;output of dispersal process
         insitu.list ; a list to identify acorns predated in situ
         decision.list1; it is a list which identifies which mobilization stopped due to low intraspecific compeition or because of too high risk perception
         decision.list2 ; it is a list which identifies if the decision between caching or predated was taken depending on distance or microhabitat
         p.risk.decisions ; proportion of times that predation risk precluded movement
         p.distance.decisions ; proportion of times that distance travelled was decisive in mouse hoarding activity
         p.microhabitat.decisions ; proportion of times that microhabitat was decisive in mouse hoarding activity
         disp.list ; contains all dispersal distances of mobilized seeds
         predation.list; list which contains final state of mobilized seeds (dispersed vs predated) 
         mean.disp ; mean dispersal distances
         max.disp ; maximum dispersal distances
         acorn5 ; to report deposited acorns within the first 5 m
         acorn10; to report deposited acorns beyond 10 m
         proportion.of.caches; to report the proportion of caches
         ] 

 patches-own [;VARIABLES RELATED TO FOREST FRAGMENTATION PROCESS
              management ; state-variable. It is used to create a central area in which no management practices can take place (preserve habitat within the study area)
              croplot ; state-variable. It is used to identify croplots and forest habitat
              forest-edge; to identify forest edges, in our base model these area patches within an area of 35 m from croplots.
              min-distance ; min-distance . It is used to identify patches which cannot be seeds of croplots (distance to the nearest croplot is below 10 m)  
              ;if miminum distance = "no" then the patch cannot act a seed of a new croplot.
              my-half-width ; from real-width-list each seed patch chooses an item.
                      ; VARIABLES RELATED TO LANDSCAPE TRAITS DESCRIPTION
                           crop-id ; to identify croplot seed
                           crop-number; to number the croplots (to obtain the number of croplots created)
                           woodlot; to identify woodlot seeds
                           woodlot-number ; to number woodlots (then obtain the number of woodlots on the landscape)
                           woodlot-area ; calculate individual woodlots area and then their values are listed in the global area-list (obtain mean area of woodlots)
                           core.edge; calculate individual woodlots core to edge ratio and then it is listed in core.edge-list (obtain mean core to edge ratio)
              
              ;;;VARIABLES RELATED TO TREE CREATION
              stem ; ("yes"/"no"). This variable is used to create the center of the canopies and also to count the number of stems created
              crop ; to model acorn production of trees
              canopy-radio ; to create the canopy
              stem-availability ; to buffer (no stems within a distance < 4)
              habitat ; oak, shrub, openland
                       ; variables related to acorn production
                       rel.max ; this is the relative to the maximum crop that a tree produces  [0.1-1](1 = tree has the maximum acorn production of the year , 0.1= tree has the minimum crop production)
              ;; VARIABLE RELATED TO MICE CREATION
              study-area; define a study area of 1 ha in the center of the world
              mice-availability ; in order to create mice home ranges and a maximum of overlapping of 30%
              mouse-here  ; in order to identify which patches have mice when identifying the trees
              tree  ; to identify tree-seed (when identifying trees in the study area)
              tree-id ; to number trees (this number wil be used in acorns (tree-id2) and mice (mouse-tree) in order to identify the acorns that are being dispersed by a target mouse
              ;created to model a correlated random-walk
              target-availability ; to spread mice movement throughout the landscape
              target-mouse ; identify which target mouse has a target patch
              distance-to-target]  
  
 
  
 mice-own [ ;VARIABLES RELATED TO SOURCE TREE-IDENTIFICATION AND THE BEGINNING OF THE DISPERSAL PROCESS
            mouse-tree ; identify the tree in which mouse warren is
            carried-acorn ; the acorn that is being dispersed
            start-patch ; so as to go back home
            initial ; in order to identify the first movement in which mice decide whether to predate in situ the acorn or not
            ;VARIABLES RELATED TO MOUSE SENSING
            number-competitors ; it counts the number of potential competitor within the mice territory
            proportion.habitat; this variable takes into account the proportion of habitat available in order to broaden compeition area when habitat availability is very low
            competition-radius ; it defines the area in which acorn mobilization is driven by intraspecific competition (competition-area)
            intra-sp-competition ; it is the ratio between competitors:acorn availability within a radius of 70 m it is used 
            prob.stop ; in order to model the probability to stop within the compeition-area
            time-since-last-cover ; internal variable which counts the number of consecutive steps a mice has moved through open microhabitats
            ;VARIABLES RELATED TO CORRELATED RANDOM WALK PROCESS
            target-patch ; the target patch towards which they move with a correlated random-walk
            my-probability; a probability to move with a random walk towards the target patch
            ] 
 
na-mice-own [nam-start-patch] 
 
acorns-own [state ; state "n" not mobilized, or dispersed or predated. if I don't specify this then mice continue moving acorns that have been already dispersed
            tree-id2 ; to identify to which tree the acorn belongs to
            a-start-patch ; acorn start patch
            my-disp  ]  ; to report dispersal distances
      
;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;;SETUP PROCEDURE;;;
;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;


;It can be divided in three different steps ;
;(1) Forest fragmentation
;(2) Create the forest
;;(3) Describe landscape
;;(4) Create mice


to setup
   ca
   ask patches
   ; at the begining of the process all patches are asked to have variables that allow them to have trees, mice warrens, being potentially managed etc.
    [set stem-availability "free"
     set mice-availability "free"
     set management "yes"
     set croplot "no"
     set woodlot ""
     set woodlot-number ""
     set crop-id ""
     set crop-number ""]
    ;; (1) FOREST FRAGMENTAITON PROCESS
     fragment-forest
    ;;;(2) FOREST HABITAT CREATION
     create-forest-habitat
    ;; (3) DESCRIBE landscape
     describe-landscape
    ;; (4) Create-the-mice
      locate-all-mice
    ;;;(5) Create-the-acorns 
     create-the-acorns 
     set prob.pred 0.25  ; this the probability of in-situ predation which is fixed to 0.27 (observed, data) 
     set crop-list []
     set insitu.list []
     set predation.list []; a list to store acorns seed fate
     set disp.list []; a list that contains distances of all mobilization events
     set competitors-list[] ; a list that
     set intra-sp-competition-list []
     set decision.list1 []
     set decision.list2 []
    reset-ticks
 ; display
end


;;;;(1) FOREST FRAGMENTATION PROCESS

to fragment-forest
     set-fragmentation-parameters
     identify-study-area
     create-croplots-half-width ; creates a list in which croplots with is defined but only taking into account values over 10 m
     create-central-woodlot ; creates a central area of 15 m of radio in which no management can take place. Ensure that the study area has forest habitat
     create-first-croplot
     create-croplots ; create croplots depending on proportion of fragmentation and agregation
     code-croplots ; code-croplots in order to identify the number of croplots created
end    

; In our base model, fragmentation (proportion of habitat loss can vary), lots-half-width is fixed to 15 and croplots are created in an aggregated way (aggregation 1)
to set-fragmentation-parameters
     set fragmentation  proportion-of-fragmentation; SLIDER  Changed during landscape sensitivity analysis
     set lots-half-width croplots-half-width; SLIDER; fixed to 15
     set agregation.index Agregation; SLIDER fixed to 1
end

to identify-study-area
   let plot-patches patch-set ( patches with [pxcor > 70 and pxcor <= 170 and pycor > 70 and pycor <= 170 ])
   ask plot-patches [set study-area "yes"]
end

  
to create-croplots-half-width
  set half-width-normal[]
  set real-half-width-list []
  repeat 100
  [let n random-normal lots-half-width 2
    set half-width-normal fput n half-width-normal ] ; list of 100 values following a normal distribution of mean 15 and SD 2
  set real-half-width-list filter [? > 10] half-width-normal ; select values higher than 10 m (croplots of an area< 800 m^2 are not allowed).
end


to create-central-woodlot ; to ensure a forest habitat within the study area (15 m corresponds to the average radius of small forest fragments in our study areas)
  ask patches with [pxcor = 120 and pycor = 120]
  [ask patches in-radius 15 [set management "no" set pcolor yellow]
    set management "no" set pcolor yellow]
end



to create-first-croplot ; we generate a first croplot in the right corner. It is important in order to define aggregation patterns.
  let p patch 220 220
  ask p
  [let n-items length real-half-width-list
   let r-item random n-items
   set my-half-width item r-item real-half-width-list
   set crop-id self
   ask patches with [pxcor < [pxcor] of myself + [my-half-width] of p and pxcor > [pxcor] of myself - [my-half-width] of p and pycor < [pycor] of myself + ([my-half-width] of p * 2) and pycor > [pycor] of myself - ([my-half-width] of p * 2) and management = "yes" and croplot = "no"]
          [set pcolor grey set croplot "yes" set crop-id [crop-id] of p set my-half-width [my-half-width] of p ] ]  
  ; Croplots shape is rectangular with width = 2*half-width and length = 4*half-width
  ; crop-id ensures that all patches created from the same seed are identified as belonging to he same croplot
end


 
to create-croplots ; then we create the rest of croplots
  let total-area count patches
  let protected.buffer 709 ; 709 corresponds to of the small forest fragment create in the procedure create-central-woodlot (above)
  let frag.area fragmentation * (total-area - protected.buffer) ; area which can be fragmented
  let potential.lots frag.area / (lots-half-width * lots-half-width * 8) ; taken into account the area potentially fragmented and croplots area we define 
  ; the number of croplots to create on the landscape
  set n-lots round potential.lots 
  show n-lots
  ;then the creation of croplots begins
  let counter 1
  loop 
[ if counter > n-lots 
    [stop]
  let p one-of patches with [management = "yes" and croplot = "no" and min-distance != "no"] ; seeds net to be arable patches, which have not been converted to croplots before and that
  ; do not hava another croplot closer than 10 m
    if p = nobody
    [stop]  
  ask p
  [let n-items length real-half-width-list
   let r-item random n-items
   set my-half-width item r-item real-half-width-list
   let nearest-croplot min-one-of other patches with [croplot = "yes"][distance myself] 
   ; seeds can only be patches with a croplot with a minimum distance to the nearest croplot of 10 
   let d distance [nearest-croplot] of p
   ; if it is closer than 10 m then it is dismissed as a potential seed and we need to discount 1 from the counter (becaouse a croplot has not been created)
   if d < 10
   [ set min-distance "no"
               set counter counter - 1
               stop]
   ;;if it is a potential seed then the agregation process begins
   ;; from each croplot there is a buffer area (my-half width) in which there is a probability of creating a new croplot is = aggregation.index
   ;; outside this belt the probability is 1 - agregation.index
   if d > 10 and d < my-half-width
             [ ifelse random-float 1 < agregation.index
               [ask p [ set crop-id self
                    ask patches with [pxcor < [pxcor] of myself + [my-half-width] of p and pxcor > [pxcor] of myself - [my-half-width] of p and pycor < [pycor] of myself + ([my-half-width] of p * 2) and pycor > [pycor] of myself - ([my-half-width] of p * 2) and management = "yes" and croplot = "no" ]
                    [set pcolor grey set croplot "yes" set crop-id [crop-id] of p set my-half-width [my-half-width] of p]]]
               [set counter counter - 1 
                 stop]]
   if d > my-half-width 
             [ifelse random-float 1 < (1 - agregation.index)
               [ask p [ set crop-id self
                 ; set plabel crop-id
                   ask patches with [pxcor < [pxcor] of myself + [my-half-width] of p and pxcor > [pxcor] of myself - [my-half-width] of p and pycor < [pycor] of myself + ([my-half-width] of p * 2) and pycor > [pycor] of myself - ([my-half-width] of p * 2) and management = "yes" and croplot = "no" ]
                    [set pcolor grey set croplot "yes" set crop-id [crop-id] of p set my-half-width [my-half-width] of p]]]
               [ set counter counter - 1
                 stop]]]
  set counter counter + 1
  ]
end


;; once all croplots have been created we need to number them, for this purpose we use the variable crop-number This way we can easily count the number of croplots that have been finally created 

 to code-croplots
   let counter 1
   loop
   [let p one-of patches with [crop-number = "" and crop-id != ""]
     if p = nobody
     [stop]
     ask p 
     [set crop-number counter
       ask patches with [crop-id = [crop-id] of myself]
       [set crop-number counter]]
    set counter counter + 1
   ] 
 end
 



;;; Now we have divided landscape into forest habitat and and croplands

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; (2) FOREST HABITAT CREATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

to create-forest-habitat
     set-forest-parameters
     create-trees
     create-understory-cover
  end


to set-forest-parameters
     set number-stems number-of-stems ;;; SLIDER. It s the maximum number of stems the world could have if all was forest habitat. Changed during landscape sensitivity analysis
     set max.crop max-crop.size ; SLIDER user defined. In our base model it is fixed to 10 (obtained from observed data; see Appendix 1.1, acorn production submodel).
     set shrub shrub.cover;; SLIDER. Changed during landscape sensitivity analysis
end



to create-trees
   create-radio-list ; creates lists with canopy radios
   create-stems ; create the stems and trees
   create-crop ; gives a crop to stems
end
 
to create-radio-list
  set normal-radio []
  set radio-list []
  repeat 200
  [let a random-normal 2.10 0.66 ; this is defined based on the data of our study area
    set normal-radio fput a normal-radio]
  set radio-list []
  set  radio-list filter [ ? > 1] normal-radio ; only values higher than 1 m are allowed
end

  

to create-stems
   loop
  [
   count-stems ; how many stems have I already created?
    if number-stems = number-of-stems ; maximum number of stems allowed in the world
    [stop]
  let p one-of patches with [stem-availability = "free" and croplot = "no"] ; inter-stems minimum distance is 4 m and they can only be placed in forest habitats
  if p = nobody
  [stop]
  ask p
  [ set stem-availability "occupied"
    set stem "yes"
    set tree self ; in order to identify all patches belonging to the same tree
    set habitat "oak"
    set tree-id "n"
    let number-items length radio-list
    let random-radio random number-items
    set canopy-radio item random-radio radio-list 
    buffer ; allow other stems to be placed in a radius of 4 m
    create-canopy
    ]
  ]
end

to count-stems
  set number-stems count patches with [stem = "yes"]
end

to buffer
  ask other patches in-radius 4 with [stem-availability = "free"]
  [set pcolor red
    set stem-availability "occupied" ]
end

to create-canopy
    ask patches in-radius ( canopy-radio ) ; 
   [set habitat "oak"
     set croplot "no"
     set pcolor green
     set tree [tree] of myself ; in order to identify oak patches of the same tree
     set tree-id "n" ]
  ;; tree-id is used to identify a specific mouse with a tree
end

;;; when creating the crop in order to avoid negative values or values above the maximum rel.maximum values are contrained to 0.1-1. 

to create-crop
 ask patches with [stem = "yes"]
  [let stems count patches in-radius 20 with [stem = "yes"]; in order to speed up the setup process we assume average canopy projections.
    let canopy.proj stems * 13  ; 13 = pi()*(2.10^2)
    let c count patches in-radius 20
    let canopy.cover canopy.proj / c
    ifelse canopy.cover > 0.6
    [set rel.max 0.1]
    [set rel.max  1.74 - (2.84 * canopy.cover)]
    if rel.max > 1
    [set rel.max 1]
    set crop rel.max * max.crop]
  end 



;;;Creating shrub cover
; The global variable shrub.cover  is refered to proportion of understory covered by shrubs and not the total
to create-understory-cover
  let total.patches count patches with [croplot = "no"]
  let cc count patches with [habitat = "oak"]
  set shrub shrub.cover
  let shrub.patches shrub.cover * (total.patches - cc) 
  let rs round shrub.patches
  ask n-of rs patches with [habitat != "oak"  and croplot = "no"]
  [set pcolor 48 set habitat"shrub"]
  ask patches with [habitat != "oak" and habitat != "shrub"]
   [set pcolor 36 set habitat "openland"]
end  

;;;;;;;;;;;;;;;
;;;;;;(3) DESCRIBE LANDSCAPE; in order to perform the landscape sensitivity analysis
;;;;;;;;;;;;;;;
 
to describe-landscape
     calculate-real-habitat-availability
     describe-croplots
     describe-woodlots  
     describe-study-area
end


to calculate-real-habitat-availability
  let f count patches with [croplot != "yes"]
  let t count patches
  set real-habitat-availability f / t
end
  

;;;;;;;;;;;;;
;;;croplots description
;;;;;;;;;;;;;;



to  describe-croplots
    set real-number-of-croplots  max [crop-number] of patches with [croplot = "yes"]
    calculate-real-croplots-half-width

end




to calculate-real-croplots-half-width
  set crop-half-width-landscape []
  let counter 1
  loop
  [if counter > real-number-of-croplots
    [set real-half-width-mean mean crop-half-width-landscape
      stop]
    let p one-of patches with [crop-number = counter]
    ask p 
    [set crop-half-width-landscape fput my-half-width crop-half-width-landscape]
     set counter counter + 1]
end 



;;;;;;;;
;;;woodlots-description
;;;;;;;;;;;

to describe-woodlots
     find-woodlots ; identify woodlots generated
     identify-edges ; identify edges (areas of high mouse abudance) 
     calculate-plots-area ; calculate woodlots area
     calculate-core-edge-ratio; calculate core to edge of woodlots
end

to find-woodlots
  find-woodlots1
  count-number-woodlots
end

;;;Firstly we identify woodlots seeds (which are chosen randomly out of those that have not been identified)
;; this process corresponds to find-woodlots1 and identify-woodlots. Once a seed is chosen woodlot identification
;; spreads trough neighbor patches that are not croplots. Once no other seeds are available then woodlots are coded in order to 
;;; extract the global variable number-of-woodlots

to find-woodlots1
  loop
  [let seed one-of patches with [croplot = "no" and woodlot = ""]
    if seed = nobody
    [code-woodlots
      stop]
    ask seed
    [set woodlot self
      identify-woodlot ]
  ]
  end
  
 to identify-woodlot
   ask neighbors4 with [croplot = "no" and woodlot = ""]
   [set woodlot [woodlot] of myself
     identify-woodlot] 
end
 
 to code-woodlots
   let counter 1
   loop
   [let p one-of patches with [woodlot-number = "" and croplot = "no" and woodlot != ""]
     if p = nobody
     [stop]
     ask p 
     [set woodlot-number counter
       ask patches with [woodlot = [woodlot] of myself]
       [set woodlot-number counter]]
    set counter counter + 1
   ] 
 end
 
 to count-number-woodlots
   set number-of-woodlots max [woodlot-number] of patches
 end 
  
 

to identify-edges
   ask patches with [croplot = "no"]
   [let my-neigh [croplot] of neighbors
     let edge length filter[? = "yes"] my-neigh
     ifelse edge > 0
     [ set forest-edge "yes"
       ask patches in-radius 35 with [croplot = "no"] ; in our base model edge-belt width is fixed to 35 m (patern-oriented model calibration)
       [set forest-edge "yes"]]
     [set forest-edge "no"]]
   end
    
   
;;;;;;;;;;;;;;;;;
;;;;Calculate woodlots area
;;;;;;;;;;;;;;;;;;;;;

to calculate-plots-area
  calculate-individual-plots-area
  create-mean-area-list
end

;; In order to define how big are the woodlots generated 

to calculate-individual-plots-area
  let counter 1
  loop
  [if counter > number-of-woodlots
    [stop]
   let area count patches with [woodlot-number = counter]
   ask patches with [woodlot-number = counter]
   [set woodlot-area area]
   set counter counter + 1]
 end 

to create-mean-area-list
  set area-list []
  let counter 1
  loop
  [if counter > number-of-woodlots
    [set mean.area mean area-list
     set max.area max area-list
      stop]
    let p one-of patches with [woodlot-number = counter]
    ask p 
    [set area-list fput woodlot-area area-list]
     set counter counter + 1]
end 
    
   
;;;;;;;;;;;;;;;;;    
;;;; calculate core-edge-ratio
;;;;;;;;;;;;;;;;;

to calculate-core-edge-ratio
  calculate-individual-core-edge-ratio
  calculate-global-core-edge-ratio
end


to calculate-individual-core-edge-ratio
    let counter 1
  loop
  [if counter > number-of-woodlots
    [stop]
   let edge count patches with [woodlot-number = counter and forest-edge = "yes"]
    let core count patches with [woodlot-number = counter and forest-edge != "yes"]
    let ce core /  edge
   ask patches with [woodlot-number = counter]
   [set core.edge ce]
   set counter counter + 1]    
end  
     
  
     
to calculate-global-core-edge-ratio
  set core.edge-list []
  let counter 1
  loop
  [if counter > number-of-woodlots
    [set mean.core.edge mean core.edge-list
      set max.core.edge max core.edge-list
      stop]
    let p one-of patches with [woodlot-number = counter]
    ask p 
    [set core.edge-list fput core.edge core.edge-list]
     set counter counter + 1]
end      
     
;;;;;;;;;;;;;;;;;;;  
;describe study area  
;;;;;;;;;;;;;;;;;;;

to describe-study-area 
   calculate-habitat-av-study-area
   calculate-core-edge-study
end

to calculate-habitat-av-study-area
  let a count patches with [croplot != "yes" and study-area = "yes"]
  let b count patches with [study-area = "yes"]
   set h.av.study a / b
end 

to calculate-core-edge-study
  let a count patches with [forest-edge != "yes" and study-area = "yes"]
  let b count patches with [forest-edge = "yes" and study-area = "yes"]
  ifelse b = 0
  [set core.edge.study "NA"]
  [ set core.edge.study a / b]
end            
 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;;     
;;;;; (4) CREATING MICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
 
 to locate-all-mice
   calculate-potential-mice-per-ha; procedure that corresponds to eq.2 of Appendix 1.2, mouse abundance submodel
   create-active-mice; , mice located close to oak trees are asked to mobilize seeds while the rest of them create a map of local mouse abundance
   identify-trees ; only trees with active mice are identified in order to safe time during the setup procedure
   identify-mouse-with-tree 
   select-target-patch; mice select a target patch towards which they move (to model correlated random-walk)
   create-the-namice ; to create a map of local mouse abundance
   create-the-na-mice-s; 
   find-prob-stop; depending on intraspecific-compeition mice select a probability to stop. Eq.5 Appendix 1.2
   find-competition-belt-width; each mice calulates the proportion of habitat available in order to adjust the competition-belt width. Eq. 4 Appendix 1.2
  end
 
 to calculate-potential-mice-per-ha; It calculates the number of mice (a priori) that we can have per ha, based on the proportion of shrub and canopy cover. Eq 2. Appendix 1.2
    let forest-patches count patches with [croplot != "yes"]
    ;show word "forest patches " forest-patches
    let safe-patches count patches with [habitat != "openland" and croplot != "yes"]
  ;  show word "safe-patches " safe-patches
    let safe-cover safe-patches / forest-patches
   ; show word "safe-cover " safe-cover
    set number-mice-prior mice.per.ha  ;SLIDER it is the maximum number of mice the landscape will have. In our base model it is fixed to 10 mice per ha, see Appendix 1.2, local mouse abundance submodel
    let numerador number-mice-prior
    let elev -1 * ((safe-cover - 0.16) / 0.07)
    let denominador 1 + (exp(elev))
    set number-mice numerador / denominador
  ;  show word "number-mice " number-mice
 end
  
 
 ;;Active mice are those whose warren is located within the study area, and they are the ones that will disperse seeds
  
 to create-active-mice
   ;random-seed new-seed
   set-default-shape mice "mouse top"
   let plot-patches patch-set ( patches with [pxcor > 70 and pxcor <= 170 and pycor > 70 and pycor <= 170 ])
   ask plot-patches [set study-area "yes"]
   let counter 1
   loop
   [ 
    if counter > number-mice 
    [stop]
    let p one-of plot-patches with [stem = "yes" and mice-availability = "free" and canopy-radio > 2.1 and mouse-here != "TRUE" and croplot = "no"]
    if p = nobody
    [stop]
    ask p [sprout-mice 1 
           [set size 4 set color black
             set initial "yes"
             set carried-acorn nobody
             set start-patch patch-here
             set time-since-last-cover 0]
            set mouse-here "TRUE"]
     let home-range 10000 / number-mice ; home-ranges shrink when there is high mouse abundance (Eq. 3.1 Appendix).
     let radio sqrt ( home-range / 3.14) ;  
     let b 0.80 * h.av.study + 0.10 ;Eq 3.2 Appendix
     ; If habitat availability is high then maximum overlap is 10%; if habitat availability is low (small forest fragments) then they can overlap between 80-90%
     let restricted-radio radio * b ; 
     ask p [ask other patches in-radius (restricted-radio) with [mice-availability = "free"]
              [set mice-availability "occupied"]]
      set counter counter + 1   
  ]
 end

  
;;Trees with active mice are identified. Then active-mouse move acorns from the tree they are attached to.
;;Within all trees in the study area N number of trees are selected to have an active mice.
;;Each tree can be considered a sample. This way we follow a procedure similar to the protocol followed in
;experimental studies in which N trees are randomly chosen within a study area. Under the canopies of these trees
;; tagged acorns are offered. 
  
to identify-trees
   let counter 1 
    loop
     [ let p one-of patches with [ tree-id = "n" and mouse-here = "TRUE"]
       if p = nobody 
        [ stop ]
       ask p [ ask patches with [tree = [tree] of myself]
       [ set tree-id counter ] ]
        set counter counter + 1
     ]
    
end
  
  
  to identify-mouse-with-tree
     ask mice [set mouse-tree [tree-id] of patch-here]
  end
  
;; target patches are patches 70 m away from starting points and are used to model the correlated random walk.

  
   to select-target-patch 
   let counter 0
   loop
   [ let nm count mice
     if counter > nm
     [stop]
     let m one-of mice with [ who = counter]
     if m = nobody
     [stop]
     ask m [set target-patch one-of patches with [distance myself = 70 and target-availability != "occupied"]]
     ask m [ask target-patch [ set target-mouse [who] of myself]]
     ask m [ask target-patch [ask other patches in-radius 15 [set target-availability "occupied"]]]
     ;target patches are asked to be spread around landscape in order to sample spatial heterogenity within the landscape
    set counter counter + 1
  ]
 end
  
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   
 ;;Creating not-active mouse in the study area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;Not active mouse (in the study area and  are important to modelate local mouse abundance

to create-the-namice
   set-default-shape na-mice "mouse top"
   let buffer-patches patch-set (patches with [study-area != "yes"])
   set na-mice-buffer number-mice * 4.76 ; correct by buffer area
   let nmb round na-mice-buffer
  let counter 1
  loop
  [
    if counter > nmb
     
    [stop]
    let p one-of buffer-patches with [ mice-availability = "free" and habitat != "openland" and mouse-here != "TRUE" and croplot = "no"]
    if p = nobody
    [stop]
    ask p [sprout-na-mice 1 
           [set size 4 set color black]
            set mouse-here "TRUE"]
     let home-range 47600 / nmb ; el home range = core-areas of home-ranges
     let radio sqrt ( home-range / 3.14)
     let habitat.in.buffer count patches  with [ crop != "yes" and study-area != "yes"]
     let total.buffer count patches with [study-area != "yes"]
     let habitat.availability2 habitat.in.buffer / total.buffer
     let b 0.80 * habitat.availability2 + 0.10 ; maximum overlap of core areas
     let restricted-radio radio * b 
     ask p [ask other patches in-radius (restricted-radio) with [mice-availability = "free"]
              [set mice-availability "occupied"]]
      set counter counter + 1     
  ]
end
 
;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; create-not-active-study Create not active mouse within the study area 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;We need to create not-active mice in order to modelate increments in mouse abundance due to shrub encroachment in savanna-like woodlands. 
 
to create-the-na-mice-s
  ; random-seed new-seed
   set-default-shape na-mice-s "mouse top"
   let study-patches patch-set (patches with [study-area = "yes"])
   let mice-study count mice
   set na-mice-study number-mice - mice-study
   let nmas round  na-mice-study
  let counter 1
  loop
  [
    if counter > nmas 
    [let c count na-mice-s
;      show word "non.active.s " c
      stop]
    let p one-of study-patches with [habitat != "openland" and mice-availability = "free"  and mouse-here != "TRUE" ]
    if p = nobody
    [let c count na-mice-s
 ;     show word "non.active.s " c
      stop]
    ask p [sprout-na-mice-s 1 
           [set size 4 set color black]
            ; set nam-start-patch patch-here
            ; set ma-cover 0]
           set mouse-here "TRUE"]
    let home-range 10000 / number-mice ; home ranges are able to shrink within reasonable bounds (15 m)
     let radio sqrt ( home-range / 3.14) ; 
     let b 0.80 * h.av.study + 0.10 ; if habitat availability is low then real-radio (which is the radius of an area occupied solely by one mice)  shrinks even more
     ; however there is a maximum core-areas of home-range overlap (represented by restricted radio) of 10% in forest interiors and of 80-90% in small forest fragments
     let restricted-radio radio * b ; 
     ask p [ask other patches in-radius (restricted-radio) with [mice-availability = "free"]
              [set mice-availability "occupied"]]
      set counter counter + 1 ]  
    
 end
 
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;Calculate the probability to stop   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Active mice are able to calculate the intraspecific-competition for acorns (competitors:crop production)
   
to find-prob-stop
  let source-list []
  ask mice [let competitors1 other mice in-radius 70 ; we set an average home-range radius of 70 which includes peripheral and core areas. Rosalino 2011
            let competitors2 na-mice in-radius 70
            let competitors3 na-mice-s in-radius 70
            let number-competitors1 count competitors1 
            let number-competitors2 count competitors2
            let number-competitors3 count competitors3
            set number-competitors number-competitors1 + number-competitors2 + number-competitors3
           ; show word "number of competitors: " number-competitors
            let potential-sources patch-set (patches in-radius 70 with [crop != 0]); in order to avoid NA. All trees produce at least 0.1 of crop.
            ask potential-sources [set source-list fput crop source-list]
            let food-availability mean source-list
            ;show word "crop availability: " crop-availability
            set intra-sp-competition (number-competitors) / food-availability
          ;  show word "intraspecific compeition :" intra-sp-competition
            set prob.stop 0.40 * exp ( -0.10 * intra-sp-competition) ;;; 0.4 = PS which was parametrized with importance-sampling. Eq. 5 Appendix 1.2
          ;  show word "prob.stop: " prob.stop10
          ;  show word "intra-sp-competition :" intra-sp-competition
            ]
end 

; THen mice are asked to calculate the porportion of habitat within their territories

to find-competition-belt-width
  ask mice [ let number.habitat count patches in-radius 70 with [croplot = "no"]
             let total  count patches in-radius 70
             let habitat.availability number.habitat / total
             ifelse habitat.availability > 0.5
             [set competition-radius 3]
             [set competition-radius 3 + 3 * (1 - habitat.availability)]] ;;; compeition-belt = CR= 3 has been parametrized with pattern-oriented approach. Eq. 4 Appendix 1.2
end
   
   
  ;;;;;;;;;;;;;;;;;;;;;;; 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
  ;;;;; (5) CREATING ACORNS
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;
  
  to create-the-acorns
    set-default-shape acorns "acorn"
    let counter 1
    loop
    [if counter > number-mice
      [stop]
      let p one-of patches with [ tree-id = counter and mouse-here = "TRUE"]
      if p = nobody
      [stop]
      ask p [sprout-acorns 200 
               [set color blue
                set size 4
                set tree-id2 [tree-id] of patch-here ; to link the acorn with the tree (tree-id) which has been previously linked to a mouse (mouse-tree)
                set state "n"
                set a-start-patch patch-here]]
      set counter counter + 1]
  end




;;;;;;;;;;;;;;;;;    
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; TO GO 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;;;;;;;;;;;;;;;;;;
 
   to go  
   tick
   set active-mouse one-of mice  
   ask active-mouse
    [; are there any un-handled acorns available?
      let A count acorns with [ tree-id2 = [mouse-tree] of myself and state = "n"]; if the mice stops dispersing the acorns then it won't continue the go procedure
       if A = 0
        [set color orange ; to see in the display whether the mouse has finished dispersing the acorn
         stop]  
      if (carried-acorn = nobody) and (initial = "yes") ; important put initial yes (this is or at the begining of the simulation or when mice go back to the warren)
         [search-acorn]
     ;in-situ predation or mobilization?
     let random-pred random-float 1 ; 
      ifelse (initial = "yes") and (random-pred < prob.pred) ; once the mouse has grabbed an acorn it has to decide whether to predate it or not at first with a predation rate
      ; prob.pred fixed to 0.25 (based on field data)
       [ask carried-acorn [set insitu.list fput who insitu.list]
         predate-acorn-in.situ]
       [set initial "no" ; if the mouse decides not to predate the acorn then it will set initial no (in order to skip the initial predation decision)
         ; and then the dispersal process begins
        disperse-acorn]
    if carried-acorn != nobody
     [ask carried-acorn [move-to myself]]; 
   ]
    ; have all the acorns been dispersed? This is importatn for the RNetLogo package
   if not any? acorns with [color = blue]
        [ make-reports
          stop ]
  end
  
  
 
 
 to search-acorn
   set carried-acorn one-of acorns with [tree-id2 = [mouse-tree] of myself and state = "n"] ; if you don't put state = "n" then the mouse will go back to dispersed acorns and recarry it.
      if carried-acorn != nobody
        [ ask carried-acorn [ht] ; I ask the acorn to hide, it will disapear until the acorn is dispersed or eaten
          set color pink] ; make it easy to detect when the mouse is carrrying an acorn
 end

 
 
  to predate-acorn-in.situ
      ask carried-acorn
      [st ; appear again
       set state "predated.in.situ"
       set color black]
   set carried-acorn nobody  ; setting carried-acorn nobody and intiial yes will make the mouse go to the begining of the go procedure
   set initial "yes" 
   set color black
end
 
;;;;
 
 to disperse-acorn
   ifelse patch-here = start-patch ; in order to return once the mobilization process finishes
  [ask active-mouse [ask neighbors [calculate-distance-target] ; this is the correlated random-walk procedure
                     create-my-list 
                     move-to-target ]
  ] 
  [let distance-s distance-source
     ifelse distance-source < competition-radius ; Am I in the competition area?
     [ifelse random-float 1 < prob.stop
      [ask active-mouse [select-destiny-competition]] ; stop mobilization process
      [ask active-mouse [ask neighbors [calculate-distance-target] ; keep moving with a correlated random-walk
                         create-my-list
                         move-to-target]]
     ]
     [if time-since-last-cover > 1 or distance-source > 70 ; 1 = Nr, risk perception thres-hold, distances beyond 70 m are not allowed (home-range limits)
       [ask active-mouse [select-destiny-risk]]; if the mouse has exceeded the risk threshold then the mobilization process stops
       ifelse (pcolor = 48) or (pcolor = green) ; if the mouse has moved towards safe microhabitats (shurbs or canopies) internal counter of time-since-last cover goes back to 0
          [set time-since-last-cover 0
           ask active-mouse [ask neighbors [calculate-distance-target] ; keep moving with a correlated random-walk
                             create-my-list
                             move-to-target]]
         
          [set time-since-last-cover time-since-last-cover + 1 ; if the mouse is moving through a risky habitat 
           ask active-mouse [ask neighbors [calculate-distance-target]
                             create-my-list
                              move-to-target]]
     ]
   ]   
 end
   
   
  to-report distance-source
   let dm distance [start-patch] of self
   report dm
 end 
 
;;;;;;;;;  
 
 ; Select destiny has been divided into select-destiny-competition and select-destiny-risk in order to track mouse decision process in decision.list1
 
to select-destiny-competition
   ifelse random-float 1 < 0.7
   [let p one-of neighbors with [habitat = "shrub" or habitat = "canopy"] ; 
     ifelse p = nobody ; if there are no neighbors with with microhabitat being shrub or canopy then mice are asked to put the acorn down
     [ask active-mouse [put-acorn-down-comp]]

     [ask active-mouse [move-to p
                        put-acorn-down-comp]]]
   [ask active-mouse [put-acorn-down-comp]]
   end  
  
  
  
to select-destiny-risk
   ifelse random-float 1 < 0.7
   [let p one-of neighbors with [habitat = "shrub" or habitat = "canopy"] ; 
     ifelse p = nobody ; if there are no neighbors with with shrub or canopy cover then mice are asked to deposit the acorn
     [ask active-mouse [put-acorn-down-risk]]

     [ask active-mouse [move-to p
                        put-acorn-down-risk]]]
   [ask active-mouse [put-acorn-down-risk]]
   end    
  
  to put-acorn-down-comp
  ask active-mouse [set decision.list1 fput "comp" decision.list1]
  ifelse [forest-edge] of patch-here != "yes" 
  [decision-core]
  [decision-edge]
  end
  
  
   to put-acorn-down-risk
  ask active-mouse [set decision.list1 fput "risk" decision.list1]
  ifelse [forest-edge] of patch-here != "yes" 
  [decision-core]
  [decision-edge]
  end
  
  ;;;; Whether to cache the seed or not depends on local mouse abundance, microhabitat of destination and distance travelled.
  ;;Our model assumes two kinds of decisions (1) in core areas it depends on the distance travelled (2) in forest edges depends on the microhabitat
  ;; of destination and the distance travelled
  
  to decision-core
    ask carried-acorn [ set my-disp [distance a-start-patch] of self
                                     set disp.list fput my-disp disp.list]
  ;acorns outside the canopy of the source tree are prone to be cached (Muñoz and Bonal, 2011)
  ifelse [tree-id] of patch-here = [mouse-tree] of myself
  [ask carried-acorn  [st 
                       set state "predated"
                       set predation.list fput state predation.list
                       set decision.list2 fput "canopy" decision.list2
                       set color black]
   set carried-acorn nobody ; I also ask the mouse to set carried-acorn nobody, to put the acorn down,  turn again black and go-back to the start-patch
   set color black
   go-back ]
  [let a [my-disp] of carried-acorn
  let prob.cached 0.75 * (1 - exp (-1 * 0.16 * a)) ; 0.75 and 0.16 have been parametrized with pattern-oriented parametrization techniques. eq 7 Appendix 1.2
  ifelse random-float 1 < prob.cached
  [ask carried-acorn [st 
                      set state "dispersed"
                      set predation.list fput state predation.list
                      set decision.list2 fput "distance" decision.list2
                      set color red]]
  [ask carried-acorn  [ st 
                       set state "predated"
                       set predation.list fput state predation.list
                       set decision.list2 fput "distance" decision.list2
                       set color black]]
   set carried-acorn nobody ;go back to the tree and begin the dispersal process
   set color black
   go-back    ]
  end
  
 
  to decision-edge
   ask carried-acorn [ set my-disp [distance a-start-patch] of self
                                     set disp.list fput my-disp disp.list]
  
   ifelse [habitat] of patch-here != "openland" ; in forest edges, acorn caching is not allowed under shrubs or canopies
           [ask carried-acorn  [st 
                       set state "predated"
                       set predation.list fput state predation.list
                       set decision.list2 fput "microhabitat" decision.list2
                       set color black]
             set carried-acorn nobody ;
             set color black
             go-back ]
           [let a [my-disp] of carried-acorn ; if it is an openland it will depend on the distance travelled
             let prob.cached 0.75 * (1 - exp (-1 * 0.16 * a))
             ifelse random-float 1 < prob.cached
                      [ask carried-acorn [st 
                      set state "dispersed"
                      set predation.list fput state predation.list
                      set decision.list2 fput "distance" decision.list2
                      set color red]]
                      [ask carried-acorn  [ st 
                       set state "predated"
                       set predation.list fput state predation.list
                       set decision.list2 fput "distance" decision.list2
                       set color black]]
    set carried-acorn nobody ; I also ask the mouse to set carried-acorn nobody, to put the acorn down,  turn again black and go-back to the start-patch
   set color black
   go-back    ]
 
  end
  
 ; procedure to ask active mouse to go to the start patch to get another acorn and clear all the internal variables 
    
 to go-back
   ;pen-up
   ask active-mouse [move-to start-patch
                     set initial "yes"
                     set time-since-last-cover 0]
 end
 

 
;;;;;; correlated random walk procedure




 to calculate-distance-target
    set distance-to-target distance [target-patch] of myself
 end
  
to create-my-list
   let my-list sort-by [[distance-to-target] of ?1 < [distance-to-target] of ?2] neighbors
   let rep length my-list
   let counter 0
   set my-probability []
   loop   ; it is important to do this loop because when mice get to the edges of the world they won't have 8 neighbors, so the number of neighbors depend on the position mice are.
  [
    if counter = length my-list
    [ stop]
    repeat rep - counter
    [set my-probability fput item counter my-list my-probability]
    set counter counter + 1 
  ]
   ; here I have created a list in which the number of repetitions is inverselly proporitonal to the position of my list; since my list is ordered by distance to target-patch 
   ; neighbors closer to target patches will have higher probability to be picked up as destiny
end

  
  to  move-to-target
    let number-items length my-probability
    let random-item random number-items
    let destiny item random-item my-probability  ; this way I stocastically 
    move-to destiny
  end
    
;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;
;;;;;;;MAKE REPORTS OF DISPERSAL PROCESS
;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;


to make-reports
   calculate-landscape-effects
   calculate-decision-statistics
   calculate-dispersal-parameters
end

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;landscape effects

to calculate-landscape-effects
  calculate-proportion-openland
  calculate-crop-comp-intraspcomp
  
end


to calculate-proportion-openland
  let ol count patches with [habitat = "openland"]
  let t count patches
  set open.land.cover ol / t
end
   
to calculate-crop-comp-intraspcomp
 ask patches with [stem = "yes"]
 [set crop-list fput crop crop-list]
  let crop.average mean crop-list
 ask mice 
 [set competitors-list fput number-competitors competitors-list]
 set comp.average mean competitors-list
 ask mice 
 [set intra-sp-competition-list fput intra-sp-competition intra-sp-competition-list]
 set intrasp.comp.average mean intra-sp-competition-list
end

;;;;;;;;;;;;;;;;;;;
;;;;;;decision statistics

to calculate-decision-statistics
  let R filter [? = "risk" ] decision.list1
  let R1 length R
  let t1 length decision.list1 ; decision list1 has recorded whether the dispersal process has finsihed due to low intraspecific ompetition for acorns or to predation risks
  set p.risk.decisions R1 / t1
  let D filter [? = "distance"] decision.list2
  let D2 length D
  let t2 length decision.list2 ; decision.list2 has recorded whether the cahing vs hoarding behavior of mice depended on distance travelled or microhabitat of destination
  set p.distance.decisions D2 / t2
  let M filter [? = "microhabitat" ] decision.list2
  let M2 length M
  set p.microhabitat.decisions M2 / t2
end


;;;;;;;;;;;;;;;;;;;;;;
;;;;;;dispersal-parameteres


to calculate-dispersal-parameters
  set mean.disp mean disp.list
  set max.disp max disp.list
  let a count acorns with [my-disp < 5 and state != "predated.in.situ"] 
  let total count acorns with [state != "predated.in.situ"]
  set acorn5 a / total
  let total.caches count acorns with [state = "dispersed"]
  let t count acorns
  set proportion.of.caches total.caches / t
end 

;;;;;BOTOMS TO SEE PLOTS


to see-kernels
  set-current-plot "Kernel of mobilized acorns"
  histogram distances
end

to-report distances
 let ad [distance a-start-patch] of acorns with [state != "predated.in.situ"]
  report ad
end


to see-proportion-of-caches
  let total.caches count acorns with [state = "dispersed"]
  let total count acorns
  set proportion.of.caches total.caches / total
  show word "proportion.of.caches :" proportion.of.caches
end  

@#$#@#$#@
GRAPHICS-WINDOW
253
10
743
521
-1
-1
2.0
1
10
1
1
1
0
0
0
1
0
239
0
239
0
0
1
ticks
30.0

BUTTON
67
19
130
52
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
956
47
1156
197
Kernel of mobilized acorns
NIL
NIL
0.0
70.0
0.0
300.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" ""

SLIDER
30
276
202
309
max-crop.size
max-crop.size
0
20.5
10
0.1
1
NIL
HORIZONTAL

SLIDER
33
323
205
356
mice.per.ha
mice.per.ha
1
18
10
1
1
NIL
HORIZONTAL

SLIDER
31
231
203
264
shrub.cover
shrub.cover
0
1
0.1
0.05
1
NIL
HORIZONTAL

SLIDER
29
186
201
219
number-of-stems
number-of-stems
0
2340
582
1
1
NIL
HORIZONTAL

SLIDER
30
102
202
135
croplots-half-width
croplots-half-width
11
30
15
1
1
NIL
HORIZONTAL

BUTTON
96
369
159
402
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
30
143
202
176
Agregation
Agregation
0
1
0.9
0.1
1
NIL
HORIZONTAL

SLIDER
19
62
225
95
Proportion-of-fragmentation
Proportion-of-fragmentation
0
1
0.9
0.05
1
NIL
HORIZONTAL

BUTTON
768
40
863
73
NIL
see-kernels
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
760
93
932
126
NIL
see-proportion-of-caches
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

It is a landscape-dispersal model. It translates management decisions into changes in key environmental factors that drive the behavior of mice and hence seed dispersal patterns.
 

## HOW IT WORKS

Forest management (forest thinning, understory removal or fragmentation) modifies local mouse abundance, acorn production of trees (the ratio between both variables determines intraspecific competition for acorns) and shelter availability for rodents. Depending on these key environmental factors mice modify their foraging behaviour and hence seed dispersal patterns.
In our model, mouse foraging decisions follow three objectives: (1) mobilize seeds away from potential competitors, (2) avoid moving through risky habitats, and (3) cache seeds in areas where the probability of pilfering by conspecifics is low. During the first meters of acorn mobilization mouse decisions are governed by intraspecific competition. Then, mobilization continues until risk perception exceeds a certain threshold. Finally when acorns are deposited, the decision between predation and seed caching depends on the risk of cache pilferage by conspecifics and the effort invested in seed mobilization (Fig. 2C).  Once carried acorns have been deposited, mice return to the source tree and the whole dispersal process restarts.  A model run finishes when all acorns within the study area are dispersed. 
See Appendix 1 for further details

## HOW TO USE IT

Model basic input- 
Amount of forest fragmentation (proportion-of-fragmentation slider) 
Stems in the world (number-of-stems,slider; the world has 5.76 ha)
Understory shrub cover (shrub.cover slider)


Additional input- 
Croplots area can be modified (slider)
Aggregation (slider) you can create a croplots in an aggregated way (agregation index 1) or dispersed in the landscape (0.1 agregation index). 
The maximum crop size of the year can be modified (slider)
Number of mice per ha (maximum) can be modified (slider)
 

## EXTENDING THE MODEL

Parameterize a more realistic random walk model in which the selection of preferential paths of movement or memory are included. This way anisotropic two-dimensional maps could be created.
Include information of the probability of seed survival depending on microhabitat of deposition.


@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

acorn
false
0
Polygon -7500403 true true 146 297 120 285 105 270 75 225 60 180 60 150 75 105 225 105 240 150 240 180 225 225 195 270 180 285 155 297
Polygon -6459832 true false 121 15 136 58 94 53 68 65 46 90 46 105 75 115 234 117 256 105 256 90 239 68 209 57 157 59 136 8
Circle -16777216 false false 223 95 18
Circle -16777216 false false 219 77 18
Circle -16777216 false false 205 88 18
Line -16777216 false 214 68 223 71
Line -16777216 false 223 72 225 78
Line -16777216 false 212 88 207 82
Line -16777216 false 206 82 195 82
Line -16777216 false 197 114 201 107
Line -16777216 false 201 106 193 97
Line -16777216 false 198 66 189 60
Line -16777216 false 176 87 180 80
Line -16777216 false 157 105 161 98
Line -16777216 false 158 65 150 56
Line -16777216 false 180 79 172 70
Line -16777216 false 193 73 197 66
Line -16777216 false 237 82 252 84
Line -16777216 false 249 86 253 97
Line -16777216 false 240 104 252 96

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

mouse top
true
0
Polygon -7500403 true true 144 238 153 255 168 260 196 257 214 241 237 234 248 243 237 260 199 278 154 282 133 276 109 270 90 273 83 283 98 279 120 282 156 293 200 287 235 273 256 254 261 238 252 226 232 221 211 228 194 238 183 246 168 246 163 232
Polygon -7500403 true true 120 78 116 62 127 35 139 16 150 4 160 16 173 33 183 60 180 80
Polygon -7500403 true true 119 75 179 75 195 105 190 166 193 215 165 240 135 240 106 213 110 165 105 105
Polygon -7500403 true true 167 69 184 68 193 64 199 65 202 74 194 82 185 79 171 80
Polygon -7500403 true true 133 69 116 68 107 64 101 65 98 74 106 82 115 79 129 80
Polygon -16777216 true false 163 28 171 32 173 40 169 45 166 47
Polygon -16777216 true false 137 28 129 32 127 40 131 45 134 47
Polygon -16777216 true false 150 6 143 14 156 14
Line -7500403 true 161 17 195 10
Line -7500403 true 160 22 187 20
Line -7500403 true 160 22 201 31
Line -7500403 true 140 22 99 31
Line -7500403 true 140 22 113 20
Line -7500403 true 139 17 105 10

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
