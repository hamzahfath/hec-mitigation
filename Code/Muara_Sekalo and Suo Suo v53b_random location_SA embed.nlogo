extensions [
py
table
nw
csv
gis
]
globals[
  product-available
  nested-table
  product_support
  peta-dasar
  data-agrikebun
  data-agriladang
  probability-map-data
  member-desa
  cum-crop-raid-incident
]


breed [farmers petani]
breed [mmedia media]
breed [plantsDB plantDB]

mmedia-own
[
  product_advertised
  adv_effectivity
  adv_mitigationType
  installment_cost
  operation_cost
  adv_product_support
  access_to_credit

]

farmers-own[
;currently used products;


        rcost-installment-cost_used
        rcost-operation-cost_used
        s-product-support_used



        measurement_effectivity_used ; turunan dari ME
          eff-aux
          effectivity-mit_1


; construct operationalization
  threat-appraisals
       t-epidemical-evidence
       t-perceived-economical-lossess
       t-crops-susceptibility

       t-treshold ; aux variable
  S-eff_i

       s-technical_dependency_i ; seberapa tinggi seseorang harus merawat produk ini dgn bantuan orang ke-3 (gabungan dari product support) (S) (dpne)
            s-auxiliary-technical_capabilities_i ; aux variable for technical dependency
            s-auxiliary-technical_dependency_i ; aux variable for technical dependency
       s-access-to-credit_i ;dependen thd product info; (done)
       s-product_awareness_i ; jumlah tetangga yang memakai suatu produk tertentu (D) ; independen thd produk info (done)
       s-education    ; tingkat pengetahuan dari suatu agen (S) ;independen thd product info; (done)
       s-income-category ; kategori pemasukan (D) ;independen thd product info;
       s-product_knowledge_i  ; kemampuan individu memakai suatu produk tertentu (D) ;independen thd produk info (done)

  M-eff_i
       ;m-mitigation-info
       m-effectivity-info ;

  R-cost_i
       r-installment_cost_info ; done
       r-operation_cost_info ; done

  intention_i

  ;buying process
  looking-for-alternatives-tools ;apakah dia looking in market for new mitigation tools (True / False)
  adoption_readiness_i
  bc_assessment_i ; benefits cost assessment of a product i
  to-be-adopted
  hold-decision?
  decision-hold-since-t

  ;tracking used product
  alternatives1_i
  past_used_tools

  last-time-adopted
  intention-treshold

  ;social network n threat information spreading
  my_firstD_neigh
  my_secondD_neigh

  attacks-memory
  attacked-at_t?
  damaged-at_t?
  damage-lvl
  attacked-list-at_t
  damaged-list-at_t

  EE-from-neighbor

  ;economical aspect of farmers
  capital_assets


  ;------------------------------------PLANT MODULE----------------------
  land-owned
  land-circumference ;to calculate the circumference of their land owned

]

undirected-link-breed [products product]

products-own [
  weight
]

patches-own [
  ;data dari gis
  desa
  agrikebun
  agriladang
  visited-probability-from-raster-data
  rng-visited-chance ;untuk memodelkan serangan gajah
  total-attacked


  ;ownership of land
  land_type
  ownership
  crop-type
  is-boundary-patches?

 ;properties of crops plant
  age ;untuk ngequery data dari db ; direset ketika tanaman telah mencapai EOL
  crop-lifetime
  crops-condition ; (1 : End of Life , 2 : Mangkrak )
  economical-value ;adalah nilai dari pohon/tanaman tersebut (biaya produksi + value dari tumbuhan)
  time-to-harvest?
  crops-ability-to-produce
  ;economical properties of crops
  potential-yield ; up to timestep t ;reset ketika dipanen
  potential-revenue ; up to timestep t ; reset ketika dipanen
  historical-data ; isinya year (PK), condition, cost, cummulative prod cost, yield, revenue, opportunity lossess

 ;HEC-Plant module
  border-patch?
  attacked?
  pot-damage-before-mit
  damage-received-after-mit


 ;mitigation implemented by the farmers
  actual_effectivity
  functionality
  mitigation-used ;formatnya number
  mit_1-effect


]

plantsDB-own [
  ;##CROPS DATA
  commodity-name
  crop-lifetime-data
  commodity-data-table ; isinya year (PK) ; ideal yield ; production cost per year ideal ;
  price ;untuk sawit 1550 per ton (Alfizar)

  ;##REPLACEMENT DATA
  replacement-cost ;merupakan bibit tanaman baru + biaya menebang pohon


]

to import-simulated-product-info

  file-open "../Data/mitigasi/product_info.csv"
  let header csv:from-row file-read-line
  let pc 0
  while [not file-at-end? ]
  [let row csv:from-row file-read-line
    create-mmedia 1
    [
      set product_advertised item 0 row
      set adv_effectivity item 1 row
      set adv_mitigationType item 2 row
      set installment_cost item 3 row
      set operation_cost item 4 row
      set adv_product_support item 5 row
      set access_to_credit item 6 row
    ]
    set pc pc + 1
  ]
  set product-available pc
  file-close-all

end

to import-plant-commodity-db

  file-close-all
  file-open "../Data/tumbuhan/data_tumbuhan_test_v5_Putri_marampa.csv"
  let header csv:from-row file-read-line
  let jenis-tanaman ["karet" "sawit" "padi"] ;jenis tanaman berdasarkan DB ; list ini perlu diupdate saat update DB
  let pc 0 ;counter

  let db table:make
      let umur (list )
      let urea (list )
      let tsp (list )
      let rl (list )
      let kcl (list )
      let kieserit (list )
      let tc (list )
      let yield (list )

  while [not file-at-end?]

  [ let row csv:from-row file-read-line
    ;print row
    ifelse is-string? item 0 row
    [
       ;(2) create one new DB
      create-plantsdb 1
      ; (3) table construction after all input gathered
      table:put db "umur" umur
      table:put db "urea" urea
      table:put db "tsp" tsp
      table:put db "rl" rl
      table:put db "kcl" kcl
      table:put db "kieserit" kieserit
      table:put db "tc" tc
      table:put db "yield" yield
      ;(4) ask the newest DB to store all info
      ask plantdb (count mmedia + pc)
      [
        set commodity-data-table db
        set commodity-name item pc jenis-tanaman
        set crop-lifetime-data (length umur) - 1 ;assigning crops lifetime
      ]
      ;(5) empty the table for the next plantDB
      set db table:make
      set umur (list )
      set urea (list )
      set tsp (list )
      set rl (list )
      set kcl (list )
      set kieserit (list )
      set tc (list )
      set yield (list )
      ;(6) set the next plantDB IDs using pc
      set pc pc + 1
    ]

    [
     ;(1) for each value given the year, assign the value into each container (list)
    set umur lput item 0 row umur
    set urea lput item 1 row urea
    set tsp lput item 2 row tsp
    set rl lput item 3 row rl
    set kcl lput item 4 row kcl
    set kieserit lput item 5 row kieserit
    set tc lput item 6 row tc
    set yield lput item 7 row yield
     ]
  ]

  file-close-all
  ;;SET PROPERTIES TO EACH COMMODITY
  ask plantsDB with [commodity-name = "karet"]
  [
     set price 1 ;price
  ]
  ask plantsDB with [commodity-name = "sawit"]
  [
    set price 1
  ]
  ask plantsDB with [commodity-name = "padi"]
  [
    set price 8
  ]
end

;----------------------------------------------------------------PETA GIS-----------------------------------------------------------------------;

to test-upload-peta
  ca
  set member-desa [ 3  7  9 ]
  resize-world -125 125 -80 80
  set peta-dasar gis:load-dataset "../Data/peta/Muara Sekalo Suo Suo - peta dasar_v1.asc"
  ;set data-agrikebun gis:load-dataset "C:/Users/Hamzah Fath/Documents/BELAJAR PETA/PETA RASTER/agrikebun_clipped_32748_100 pixel_asc.asc"
  ;set data-agriladang gis:load-dataset "C:/Users/Hamzah Fath/Documents/BELAJAR PETA/PETA RASTER/agriladang_clipped_32748_100 pixel_asc.asc"
  set probability-map-data gis:load-dataset "../Data/peta/Muara Sekalo Suo Suo - raster probability map_v1.asc"
  gis:set-world-envelope gis:envelope-of peta-dasar

 gis:apply-raster peta-dasar desa
 ;gis:apply-raster data-agrikebun agrikebun
 ;gis:apply-raster data-agriladang agriladang
 gis:apply-raster probability-map-data visited-probability-from-raster-data

  ;adjustment for bleeding data
  ask patches with [ (desa >= 0 or desa <= 0) and not (visited-probability-from-raster-data >= 0 or visited-probability-from-raster-data <= 0)][set visited-probability-from-raster-data 0]
  ask patches with [not (desa >= 0 or desa <= 0) and (visited-probability-from-raster-data >= 0 or visited-probability-from-raster-data <= 0)][set desa 0]
  set-patch-size 2
  ask patches with [desa >= 0] [set pcolor scale-color green desa .01 10
]
 ; give-visual


end

to give-visual
  ;patches with [member? desa jumlah-desa] --> untuk menempatkan desa
  ;ask patches with [agrikebun != 0 and agriladang != 0 and desa != 1 ][set pcolor desa ]
  ask patches with [ agrikebun = 1 ][set pcolor blue ]
  ask patches with [agriladang = 1] [set pcolor green]
 ; ask patches with [agrikebun = 0 and agriladang = 0 and desa = 1 ][set pcolor desa ]
  ;ask patches with [agriladang != 0][set pcolor red]

end

;----------------------------------------------------------------PETA GIS-----------------------------------------------------------------------;

to spatial-clustered-network-configuration-setup
  let num-links (average-node-degree * count farmers ) / 2
  while [count products < num-links ]
  [
    ask one-of farmers
    [
      let choice (min-one-of (other farmers with [not link-neighbor? myself])[distance myself])
      if choice != nobody [ create-product-with choice [
          set thickness .5
          set color magenta] ]
    ]
  ]

  ask farmers[
    set my_firstD_neigh turtle-set [other-end] of my-products                           ;to define the set agentthe first order neighbors of farmers
    set my_secondD_neigh turtle-set [[other-end] of my-links] of my_firstD_neigh     ;to define the agentset the second order neighbors of farmers (friends of my friends)
    set my_secondD_neigh my_secondD_neigh with [who != [who] of myself]              ;remove the calling farmers from the second degree links
  ]
end

to innitiate-product-knowledge ;for product_knowledge_i

    ask farmers
    [
      let product_key 1
      let product_ME_info table:make
      let training_i table:make
      while [product_key <= product-available][
        let k random-float 1
        table:put training_i product_key k
        set product_key product_key + 1
     ]
      set s-product_knowledge_i training_i

    ]

   ask farmers
    [
      let product_key 1
      let adoption_readiness table:make

      while [product_key <= product-available][
        let k random-float 1
        table:put adoption_readiness product_key 0
        set product_key product_key + 1
     ]
      set adoption_readiness_i adoption_readiness

    ]
end

to innitiate-threat-memory ;for threat appraisals (1)
  ask farmers
  [
  let memory table:make
    table:put memory "attacks" [[0]]
    table:put memory "damaged?" [[0]]
    table:put memory "damage-level" [[0]]
    table:put memory "neigh-helping?" [[0]]
    table:put memory "using-pm?" [[0]]
    table:put memory "pm-used" [[0]]
    set attacks-memory memory
  ]
end

to setup-used-product-spesification ; for R-Cost I II and SE product_dependency
let product_key 1
  while [product_key <= product-available]
  [
    ask farmers with [alternatives1_i = product_key]
    [
      set rcost-installment-cost_used item 0 [installment_cost] of mmedia with [product_advertised = product_key]
      set rcost-operation-cost_used item 0 [operation_cost] of mmedia with [product_advertised = product_key]
      set s-product-support_used item 0 [adv_product_support] of mmedia with [product_advertised = product_key]

    ]

   set product_key product_key + 1
  ]
end

to configure-innitial-adoption
  let proportion-0 .90
let proportion-1  .01
let proportion-2 .03
let proportion-3 .06

let num-turtles count turtles


ask n-of (farmers-population * proportion-1) farmers with [ alternatives1_i = -1 ] [ set alternatives1_i 1 ]
ask n-of (farmers-population * proportion-2) farmers with [ alternatives1_i = -1 ] [ set alternatives1_i 2 ]
ask n-of (farmers-population * proportion-3) farmers with [ alternatives1_i = -1 ] [ set alternatives1_i 3 ]
  ask farmers with [alternatives1_i = -1]  [ set alternatives1_i 0 ]
end

to generate-farmers
 ; file-close-all
  ;file-open "D:/Onedrive/Skripsi/Thesis/Netlogo Code/Posisi Petani V1_csv.csv"
  ;let header csv:from-row file-read-line
  ;while [not file-at-end?][
   ; let row csv:from-row file-read-line
  while [count farmers < farmers-population][
create-farmers 1 [
      let k one-of patches with [desa = one-of member-desa]
      setxy [pxcor] of k [pycor] of k
     ; setxy item 0 row item 1 row
      while [any? other farmers in-radius 4 and not member? [desa] of patches in-radius 4 member-desa ]
            [set k one-of patches with [desa = one-of member-desa]
            setxy [pxcor] of k  [pycor] of k ] ;]
    ;create-products-to other farmers
    set alternatives1_i -1

;used product

        set measurement_effectivity_used random-float 1
        set eff-aux innitiate-effectivity-probability-mitigation-list
        set past_used_tools []


;----------------INTENTION ATTRIBUTE INNITIATION-----------------------------------;
;static variable && independent to product
        set s-education 1 + random 3
        set s-income-category 1 + random 3

;static variable && dependent to product
        set s-access-to-credit_i generate-level_1-product-table 0 True
        set s-product_knowledge_i generate-level_1-product-table 1 False
        set s-technical_dependency_i generate-level_1-product-table 0 True
        set s-auxiliary-technical_capabilities_i generate-level_1-product-table 4 True

;candidate product information gathered from various source
        set m-effectivity-info  innitiate-master-table-product-info
        set s-auxiliary-technical_dependency_i innitiate-master-table-product-info

        set s-product_awareness_i count-product-availability my_firstd_neigh

        set r-installment_cost_info innitiate-master-table-product-info
        set r-operation_cost_info innitiate-master-table-product-info

;intention operationalization variable
        set adoption_readiness_i generate-level_1-product-table 0 False
        set looking-for-alternatives-tools False ;by default orang ga looking for alternative products on market
        set intention_i generate-level_1-product-table 0 False
        set intention-treshold random-float 1
        set hold-decision? False

;threat operationalization variable
        set t-treshold .2 + random-float .3
;----------------DEMOGRAPHIC ATTRIBUTE INNITIATION----------------------------------;
;demographic initialization
        set capital_assets random-normal avg-wealth  std-wealth ; 1 T mean 30 jt deviation
  ]]
  setup-used-product-spesification
  ask products [set weight random-float 1]
 configure-innitial-adoption
end


to generate-crops ;mitigation-used perlu ditambah di prosedur go
  ;land ownership dapat di embed di file GIS dengan menambahkan feature land-owner
  ask farmers [
    let k patches in-radius 1 with [ownership = 0 and desa <= 10 ]
    let jenis-tanaman one-of ["karet" "sawit"]
    ask k [
      set ownership myself
      set pcolor green
      set Land_Type "agriculture"
      set crops-condition 100
      set crop-type jenis-tanaman
      set is-boundary-patches? update-boundaries-patch neighbors4

      set mitigation-used  [alternatives1_i] of myself  ;menggunakan list procedure untuk mencegah literal value error
       ;constructing table for elephant attack history;;;
      set mit_1-effect innitiate-damage-reduction-mit-aux


      set historical-data innitiate-commodity-historical-yield-table


    ]
   set land-owned k
   set land-circumference assess-perimeter self
  ]
  ask patches with [Land_Type != "agriculture"][set mitigation-used -999999]
end
;#################################################### TRY PERFORM SA IN NETLOGO #########################################################;
to perform-SA
  py:setup py:python
  (py:run
"import os, glob"
"for f in glob.glob('../Model_output/sensitivity_analysis/*.txt'):"
"     os.remove(f)")
  ; 'attacks-memory-len',
   ;               'average-node-degree',
    ;              'between-adoption-time-delay',
     ;             'avg-wealth',
      ;            'std-wealth',
       ;           'mean-attack',
        ;          'std-attack',
         ;         ],
  let params-master-list morris-params-gen
  ;###START ON PERFORMING GO WITH DESIGNATED PARAMS

  foreach params-master-list [ params-set ->
    ;# Asign params to variable value
    set attacks-memory-len item 1 params-set
    set average-node-degree item 2 params-set
    set between-adoption-time-delay item 3 params-set
    set avg-wealth item 4 params-set
    set std-wealth item 5 params-set
    set mean-attack item 6 params-set
    set std-attack item 7 params-set
    set top-down-success-reach-treshold item 8 params-set

    ;#record foreach number
    let run-number position params-set params-master-list
    ;#create a new file
    let filename (word "../Model_output/sensitivity_analysis/reach_loops_" run-number ".txt")

    ;setup the program by given parameter
    setup
    repeat 40 [
      go
       py:run (word "with open('" filename "', 'a') as f: f.write(str(" count farmers with [alternatives1_i = 1] ") + ',' + str(" count farmers with [alternatives1_i = 2] ") + ',' + str(" count farmers with [alternatives1_i = 3] ") + ',' + str(" count farmers with [alternatives1_i = 0] ") + '\\n')")

    ]
    ;close the given files


  ]


end
;####################################################END OF TRY PERFORM SA IN NETLOGO #########################################################;
to setup
 ;----------------------------------------------------------------------------------------------------;
 ;------Innitiation----------------------------------------------------------------------------------;
    ca


  py:setup py:python
  py:run "import pandas as pd"
 ; py:run "import bnlearn as bn"
  ca
  reset-ticks
 ;----------------------------------------------------------------------------------------------------;
 ;------IO configuration------------------------------------------------------------------------------;
  test-upload-peta
  import-simulated-product-info
  import-plant-commodity-db

  ;----------------------------------------------------------------------------------------------------;
  ;------POPULATION GENERATION------------------------------------------------------------------------;

  generate-farmers
  generate-crops

  ;----------------------------------------------------------------------------------------------------;
  ;------Construct Innitiation-------------------------------------------------------------------------;
  innitiate-threat-memory
  update-mit-used-by-farmers
  ;innitiate-product-knowledge

  ;----------------------------------------------------------------------------------------------------;
  ;----------------------------------------------------------------------------------------------------;



  ;----------------------------------------------------------------------------------------------------;
  ;------SOCIAL NETWORK CONFIG-------------------------------------------------------------------------;
  spatial-clustered-network-configuration-setup


  ;----------------------------------------------------------------------------------------------------;
  ;----------------------------------------------------------------------------------------------------;
reset-ticks


end

;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################
;##############################################GO PROCEDURE###########################################################

to go
  clear-output

  refresh-visuals-and-state-var
  ;----------------------------------------------------------------------------------------------------;
  ;------KGM RELATED-----------------------------------------------------------------------------------;
  mitigation-bill
  simulate-HEC

  ;----------------------------------------------------------------------------------------------------;
  ;------PLANT RELATED---------------------------------------------------------------------------------;

  update-crops-attributes
  ;----------------------------------------------------------------------------------------------------;
  ;-----SOCIAL NETWORK CONFIGURATION-------------------------------------------------------------------;
   top-down-communication-process
  ;----------------------------------------------------------------------------------------------------;
  ;------CONSTRUCT RELATED-----------------------------------------------------------------------------;


  ;Threat Operationalization (Gather data from various info);
  ;(1) Epidemical Evidence
  record-self_EE
  record-EE-from-neighbors
  record-bayesian-epidemical-evidence
  ;(2) Economical Lossess
  record-economic-lossess-from-HEC-on-crops
  ;(3) Crops-succeptibility
  assess-crops-succeptibility-based-on-age

  ;Intention Process CALCULATION
  ;(PART 0)
  calculate-threat-appraisals
  assess-farmers-threat-treshold  ;setelah melampaui treshold baru memulai threat process
  ;(PART 1)
  gather-info-for-operationalizing-construct
  ;(PART 2)
  operate-pmt-construct
  ;(PART 3) ; melakukan decision apakan akan acquire new mitigation tools (lagi) atau tidak.

  ;updating farmer's decision state-variables
  update-mit-used-by-farmers
  bc-assessment
  adoption-action2




   ;----------------------------------------------------------------------------------------------------;
   ;----------------------------------------------------------------------------------------------------;



   ;----------------------------------------------------------------------------------------------------;
   ;----------------------------------------------------------------------------------------------------;

   ;----------------------------------------------------------------------------------------------------;
   ;-----RECORD OUTPUT----------------------------------------------------------------------------------;


   ;----------------------------------------------------------------------------------------------------;
   ;----------------------------------------------------------------------------------------------------;
  tick
end

;;;;;;;;;;;;;;;;;;SUB GO PROCEDURE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to operate-pmt-construct
  measurement-efficacy-calculation ;NOTE MUST BE ADJUSTED FOR MULTIPLE ADOPTION
  Self-efficacy-calculation   ;NOTE MUST BE ADJUSTED FOR MULTIPLE ADOPTION
  response-cost-calculation  ;NOTE MUST BE ADJUSTED FOR MULTIPLE ADOPTION
  intention-calculation
  assess-adoption-readiness
end

to gather-info-for-operationalizing-construct
  ;Measurement efficacy operationalization;
  record-product-spesification-from-neighbors ; for recording ME_2 RC_1 RC_2 NOTE MUST BE ADJUSTED FOR MULTIPLE ADOPTION

  ;Self Efficacy Operationalizaiton;
  update-product-knowledge ; for recording SE_6
  reassess-technical-dependency

  ;Response cost operationalization;


  ask farmers[


    set s-product_awareness_i count-product-availability my_secondd_neigh ; for recording SE_3 NOTE MUST BE ADJUSTED FOR MULTIPLE ADOPTION
    ;set threat-appraisals t-epidemical-evidence
 ]
end

;############## END OF SUB GO PROCEDURE ###############################################

;;;;;;;;;;;;;;;;;;HUMAN INTENTION OPERATIONALIZATION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to record-product-spesification-from-neighbors

  ask farmers with [looking-for-alternatives-tools = True]
    [
      let product_key 1
      let product_effectivity_info table:get m-effectivity-info "neigh"
      let product_installation_info table:get r-installment_cost_info "neigh"
      let operation_cost_info table:get r-operation_cost_info "neigh"
      let technical_dependency_auxiliary table:get s-auxiliary-technical_dependency_i "neigh"


      while [product_key <= product-available][

        let measurement-effectivity [] ;for measurement efficacy construct (1)
        let installation-cost [] ; for response cost construct (1)
        let operation-cost []   ;for Response cost construct (2)
        let technical_dependency [] ; for self efficacy (1)




        foreach sort (my_firstD_neigh)  ;revisit
          [x ->
            if [alternatives1_i]  of x = product_key [
              set measurement-effectivity lput  [effectivity-mit_1 ] of x  measurement-effectivity
              set installation-cost lput  [rcost-installment-cost_used  ] of x  installation-cost
              set operation-cost lput  [rcost-operation-cost_used ] of x  operation-cost
              set technical_dependency lput [s-product-support_used] of x technical_dependency

                    ]
            ]

        ;WORKS FOR  CODE FOR MULTIPLE ADOPTION SECTION INFORMATION GATHERING


        ifelse not empty? measurement-effectivity
        [table:put product_effectivity_info product_key mean measurement-effectivity]
        [table:put product_effectivity_info product_key 0]


         ifelse not empty? installation-cost
        [table:put product_installation_info product_key one-of installation-cost]
        [table:put product_installation_info product_key 0]

         ifelse not empty? operation-cost
        [table:put operation_cost_info product_key one-of operation-cost]
        [table:put operation_cost_info product_key 0]

        ifelse not empty? technical_dependency
        [table:put technical_dependency_auxiliary product_key one-of technical_dependency]
        [table:put technical_dependency_auxiliary product_key 0]

        set product_key product_key + 1
     ]
      set m-effectivity-info  put-nested-table-lvl1 m-effectivity-info "neigh" product_effectivity_info
      set r-installment_cost_info  put-nested-table-lvl1 r-installment_cost_info "neigh" product_installation_info
      set r-operation_cost_info  put-nested-table-lvl1 r-operation_cost_info "neigh" operation_cost_info
      set s-auxiliary-technical_dependency_i  put-nested-table-lvl1 s-auxiliary-technical_dependency_i "neigh" technical_dependency_auxiliary



    ]

end

to update-product-knowledge ; S_Eff operationalization
  let product_training-type 1 + random product-available
  let training-occassion random 2


  ask farmers with [looking-for-alternatives-tools = True] [
      let product_key 1
      while [product_key <= product-available]
      [
        if product_training-type = product_key
        [
          ifelse table:get s-product_knowledge_i product_key < 1
              [ table:put s-product_knowledge_i product_key (table:get s-product_knowledge_i product_key + training-occassion * .1)]
              [ table:put s-product_knowledge_i product_key 1 ]
        ]

        set product_key product_key + 1
      ]

    ]

end

to reassess-technical-dependency ;NOTE MUST BE ADJUSTED FOR MULTIPLE ADOPTION
  ask farmers with [looking-for-alternatives-tools = True] [
    let product_key 1
    let a table:make


    while [product_key <= product-available]
  [
     let current_tech_dependency value-mapper (table:get s-auxiliary-technical_capabilities_i product_key) "s-auxiliary-technical_capabilities_i"
     let candidate_info value-mapper (select-one-of-info (get-nested-table s-auxiliary-technical_dependency_i "media" product_key)
      (get-nested-table s-auxiliary-technical_dependency_i"neigh" product_key)) "s-auxiliary-technical_dependency_i"

     ifelse current_tech_dependency - candidate_info > 0
      [let future_tech_dependency (current_tech_dependency - candidate_info)
      table:put a product_key future_tech_dependency
      ]

      [let future_tech_dependency 0
       table:put a product_key future_tech_dependency]






  set product_key product_key + 1]

    set s-technical_dependency_i a
  ]
end

to measurement-efficacy-calculation


  let coefME_2  1

  let w_from_media 1
  let w_from_neigh 1


  ask farmers with [looking-for-alternatives-tools = True] [
      let ME_dummy table:make
      let product_key 1
      while [product_key <= product-available]
      [
        table:put ME_dummy product_key
        (

         (
            ((w_from_media * get-nested-table m-effectivity-info "media" product_key) +
            (w_from_neigh * get-nested-table m-effectivity-info "neigh" product_key ) ) / sum (list w_from_media w_from_neigh)

          ) * coefME_2
        )


        set product_key product_key + 1
      ]
      set M-eff_i ME_dummy
    ]

end

to Self-efficacy-calculation

  let coefSE_1   [0 0 0]  ;item 0 pagar item 1 api item 2 bunyi
  let coefSE_2   [.5 .33 0]
  let coefSE_3   [0 .33 0]
 ;let coefSE_4   [0 1 0]
  let coefSE_5   [.5 .33 0]
  let coefSE_6   [0 0 0]




  ask farmers with [looking-for-alternatives-tools = True][
      let SE_dummy table:make
      let product_key 1
      while [product_key <= product-available]
      [
        table:put SE_dummy product_key
            (
                  ; table:get s-technical_dependency_i product_key * coefSE_1 +
                   value-mapper (table:get s-access-to-credit_i product_key) "s-product_knowledge_i" * item (product_key - 1) coefSE_2 +
                   value-mapper (table:get s-product_awareness_i product_key) "s-product_awareness_i" * item (product_key - 1) coefSE_3 +
                   value-mapper (table:get s-product_knowledge_i product_key) "s-product_awareness_i"  * item (product_key - 1) coefSE_6


     )


        set product_key product_key + 1
      ]
      set S-eff_i SE_dummy
    ]

    ; s-technical_dependency_i ; seberapa tinggi seseorang harus merawat produk ini dgn bantuan orang ke-3 (gabungan dari product support) (S)

       ;s-access-to-credit_i ;independen thd product info;
      ; s-product_awareness_i ; jumlah tetangga yang memakai suatu produk tertentu (D) ; independen thd produk info
       ;s-education    ; tingkat pengetahuan dari suatu agen (S) ;independen thd product info;
       ;s-income-category ; kategori pemasukan (D) ;independen thd product info;
      ; s-product_knowledge_i  ; kemampuan individu memakai suatu produk tertentu (D) ;independen thd produk info
end

to Response-cost-calculation

  let coefRC_1  [.5 0 0]
  let coefRC_2  [.5 1 0]

  ask farmers with [looking-for-alternatives-tools = True] [
      let RC_dummy table:make
      let product_key 1
      while [product_key <= product-available]
      [
        table:put RC_dummy product_key
        (
          value-mapper ( select-one-of-info (get-nested-table r-installment_cost_info "media" product_key)  (get-nested-table r-installment_cost_info"neigh" product_key)) "r-installment_cost_info" * item (product_key - 1) coefRC_1 +
          value-mapper (select-one-of-info (get-nested-table r-operation_cost_info "media" product_key)  (get-nested-table r-operation_cost_info"neigh" product_key)) "r-operation_cost_info" * item (product_key - 1) coefRC_2

        )


        set product_key product_key + 1
      ]
      set R-cost_i RC_dummy
    ]

end

to intention-calculation
  let constant [-.0147 0 .2885]
  let CoefTA [.18 .16 .52]
  let CoefME [.13 .23 .205]
  let CoefRC [.176 .21 0]
  let CoefSE [.66 .75 0]
  ask farmers with [looking-for-alternatives-tools = True][
      let intention_dummy table:make
      let product_key 1
      while [product_key <= product-available]
      [
        table:put intention_dummy product_key
        (table:get  S-eff_i product_key * (item (product_key - 1) CoefSE)+
        table:get R-cost_i product_key * (item (product_key - 1) CoefRC) +
        table:get M-eff_i product_key * (item (product_key - 1) CoefME) +
        threat-appraisals * (item (product_key - 1) CoefTA) +
        item (product_key - 1) constant)


        set product_key product_key + 1
      ]
      set intention_i intention_dummy
    ]

end

to assess-adoption-readiness

  ask farmers with [looking-for-alternatives-tools = True] [
      let adoption_dummy table:make
      let product_key 1
      while [product_key <= product-available]
      [
        ifelse table:get intention_i product_key > intention-treshold
        [ table:put adoption_dummy product_key 1]
        [ table:put adoption_dummy product_key 0]



        set product_key product_key + 1
      ]
      set adoption_readiness_i adoption_dummy
    ]

end

;;;;;;;;;;;;;;;;          DECISION MODELING         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to update-mit-used-by-farmers
  ask farmers [

    ask patches with [ownership = myself ]
    [set mitigation-used [alternatives1_i] of myself  ]
  ]


end
to bc-assessment ;perlu direvisit setelah mempertimbangkan multiple adoption
  ;to decide which product should be adopted using heuristical method --> assess B/C nya | cari yang paling bisa fulfill |
  ; cari yang B/Cnya yang paling tinggi
  ; if dalam adoption interval ga ketemu; cari yang paling affordable given the highest B/C

  ask farmers with [looking-for-alternatives-tools = True] [
    let bc_dummy table:make
    let product_key 1
    let w_from_media 1
    let w_from_neigh 1
    while [product_key <= product-available] [
      ;b/c calculation
     ifelse table:get adoption_readiness_i product_key = 1 [
      let benefits_i
      ((w_from_media * get-nested-table m-effectivity-info "media" product_key)  +
              (w_from_neigh * get-nested-table m-effectivity-info "neigh" product_key)) / sum (list w_from_media w_from_neigh)
      let cost_i
       ( select-one-of-info (get-nested-table r-installment_cost_info "media" product_key)
        (get-nested-table r-installment_cost_info"neigh" product_key))
    carefully [
          table:put bc_dummy product_key (benefits_i / cost_i)] [table:put bc_dummy product_key 0]
        ]
      [table:put bc_dummy product_key 0 ]


     set product_key product_key + 1
    ]
    set bc_assessment_i bc_dummy
  ]

  ;note ::
  ; KONSEP BC ASSESSMENT UNTUK MULTIPLE ADOPTION BISA DISETTING BERDASARKAN RASIO INTENTION TO PRICE YANG PALING TINGGI OR SECOND HIGHEST ETC.
end


to adoption-action2; reassess

;note --> behaviour repurchasenya :
  ;if pagar listrik --> cannot adopt another tools installation cost sekali; operation cost setiap 4 ticks (1 thn)
  ;if meriam --> setiap diserang --> installation dilakukan setiap diserang gajah ; baru bisa beli lagi setelah diserang
 ; if api unggun --> sama seperti meriam
  ask farmers with [looking-for-alternatives-tools = True][
   let purchased-mitigation alternatives1_i
   ;let candidate 0


  if hold-decision?  = False [
      ifelse last-time-adopted = 0 [adopt-hd-false]
      [if last-time-adopted + between-adoption-time-delay < ticks
        [adopt-hd-false]
      ]

      ]

    if hold-decision? = True [

    let c capital_assets
    ifelse c > (select-one-of-info (get-nested-table r-installment_cost_info "media" to-be-adopted)  (get-nested-table r-installment_cost_info"neigh" to-be-adopted))
    [
    ;(2) choosing which mitigation should be placed (apakah di mit_1, mit_2, atau mit_3)
          if alternatives1_i = 0
              [
              set alternatives1_i to-be-adopted
              set past_used_tools lput alternatives1_i past_used_tools
                set last-time-adopted ticks
                set rcost-installment-cost_used [installment_cost] of mmedia with [product_advertised = alternatives1_i]
                set rcost-operation-cost_used [operation_cost] of mmedia with [product_advertised = alternatives1_i]
                set s-product-support_used [adv_product_support] of mmedia with [product_advertised = alternatives1_i]

          ]


    set c c - (select-one-of-info (get-nested-table r-installment_cost_info "media" to-be-adopted)  (get-nested-table r-installment_cost_info"neigh" to-be-adopted))
      set hold-decision? False
    ]
    [set decision-hold-since-t decision-hold-since-t + 1]

    ]


  ]
   setup-used-product-spesification




end

to adopt-hd-false
      let c capital_assets
    ;(0) sorting candidates based on BC assessment (from highest BC to lowest)
      let candidates_table return_descending_table_based_on_values bc_assessment_i
;-----------------------------------------IF already made previous purchases----------------------------------------------------;
    ;(1) choosing candidate that hasn't been adopted with considering their used tools and their adoption readiness
        let table_comparison []
      foreach candidates_table
      [key-value-pair ->
         set table_comparison lput (list item 0 key-value-pair ((table:get m-eff_i item 0 key-value-pair) - effectivity-mit_1) (item 0 [installment_cost] of mmedia with [product_advertised = item 0 key-value-pair]) ) table_comparison  ;dikomparasi efektivitas dari tools yang ada dengan miliknya
      ]
     ; print (table_comparison)
       let sorted_table_comparison return_descending_table_based_on_values table_comparison
      ;print (table_comparison)
       let candidate_and_price constrained_effectiveness_assessment sorted_table_comparison (capital_assets - .1 * capital_assets)

       ;checking if the highest effectiveness is already used
      ifelse item 0 candidate_and_price = 0 [][
       ifelse item 0 candidate_and_price = alternatives1_i
        [] ;do nothing if the highest effectiveness is already used
        [ ifelse c > item 1 candidate_and_price
          [set alternatives1_i item 0 candidate_and_price
           set c c - item 1 candidate_and_price
        set last-time-adopted ticks
           set past_used_tools lput alternatives1_i past_used_tools
          set hold-decision? False
          ]
          [set to-be-adopted item 0 candidate_and_price
           set hold-decision?  True
           set decision-hold-since-t decision-hold-since-t + 1 ] ;hold decision dibikin fungsi baru supaya bisa bedain antara tools yang sama dengan tool yang beda untuk to be adoptednya
        ]
      ] ;purchase if had enough capital

end
;----------- END OF DECISION MODELING  ---------------------------------;
;-----------END OF HUMAN INTENTION OPERATIONALIZATION ------------;

;;;;;;;HUMAN THREAT OPERATIONALIZATION ;;;;;;;;;;;;;;;;;;;;;;;;;;


to record-self_EE ; threat operationalization

    let attacked-list-at_t_dummy []
    let damaged-list-at_t_dummy []
    let pm_used_list []
    ask farmers [


    ifelse any? land-owned with [attacked? = 1 ]
    [
    foreach [self] of land-owned with [attacked? = 1]
    [x -> ask x [
      set attacked-list-at_t_dummy lput attacked? attacked-list-at_t_dummy
      set damaged-list-at_t_dummy lput last (table:get mit_1-effect "residual") damaged-list-at_t_dummy]
      set pm_used_list lput mitigation-used pm_used_list ]
      ]
     [ set attacked-list-at_t_dummy lput 0 attacked-list-at_t_dummy
       set damaged-list-at_t_dummy lput 0 damaged-list-at_t_dummy
       set pm_used_list lput mitigation-used pm_used_list
    ]

    set attacked-list-at_t attacked-list-at_t_dummy
    set damaged-list-at_t damaged-list-at_t_dummy
    ;set pm_used_list

let len attacks-memory-len

    let a table:get attacks-memory "attacks"
   let d? table:get attacks-memory "damaged?"
   let d-lvl table:get attacks-memory "damage-level"

   let pm table:get attacks-memory "using-pm?"
   let pm-used table:get attacks-memory "pm-used"

    ifelse length a < attacks-memory-len
    [set a lput attacked-list-at_t a ]
    [set a bf a
     set a lput attacked-list-at_t a
    ]

    ifelse length d? < attacks-memory-len
    [set d? lput binary_assessment damaged-list-at_t d? ]
    [set d? bf d?
     set d? lput binary_assessment damaged-list-at_t d?
    ]

     ifelse length d-lvl < attacks-memory-len
    [set d-lvl lput damaged-list-at_t d-lvl ]
    [set d-lvl bf d-lvl
     set d-lvl lput damaged-list-at_t d-lvl
    ]


      ifelse length pm < attacks-memory-len
    [set pm lput binary_assessment pm_used_list pm
     set pm-used lput pm_used_list pm-used ]
    [set pm bf pm
      set pm-used bf pm-used
      set pm lput binary_assessment pm_used_list pm
      set pm-used lput pm_used_list pm-used
    ]

    table:put attacks-memory "attacks"  a
    table:put attacks-memory "damaged?"  d?
    table:put attacks-memory "damage-level"  d-lvl

    table:put attacks-memory "using-pm?"  pm
    table:put attacks-memory "pm-used"  pm-used
  ]

  ;ask farmer5 [show (list attacks-memory attacked-at_t? damaged-at_t?) ]


end

to record-EE-from-neighbors ;threat operationalization
  let farmer-counter (product-available + count plantsDB)

   while [farmer-counter < count farmers + product-available + count plantsDB ] [
    ask petani farmer-counter [
      set EE-from-neighbor my-neighbor-EE
    ]

   set farmer-counter farmer-counter + 1
    ]


end

to record-bayesian-epidemical-evidence
  if ticks > 5 [
    ask farmers[set t-epidemical-evidence operate-bnlearn EE-from-neighbor]
  ]
end

to record-economic-lossess-from-HEC-on-crops
  ask farmers [
  let lossess-list 0
    carefully [set lossess-list [sum table:get historical-data "opportunity_lossess"] of land-owned ] [set lossess-list 0]
    let sum-lossess-list sum (lossess-list)
    set t-perceived-economical-lossess sum-lossess-list
  ]
end

to assess-crops-succeptibility-based-on-age
  ask farmers [
    let k median ([age] of patches with [ownership = myself])
    carefully [set t-crops-susceptibility (e ^ (-.15 * k))][set t-crops-susceptibility 1 ]
    ; set t-crops-susceptibility value-mapper t-crops-susceptibility "t-crops-susceptibility"
  ]
end

to calculate-threat-appraisals
  let a1 TA1
  let a2 TA2
  let a3 TA3
  ask farmers [
    set threat-appraisals
    ( a1 *  t-crops-susceptibility  +
       a2 *  t-epidemical-evidence  +
       a3 *  value-mapper "t-perceived-economical-lossess"  t-perceived-economical-lossess )
  ]
end

to assess-farmers-threat-treshold
  ask farmers [
    ifelse threat-appraisals < t-treshold
    [set looking-for-alternatives-tools False]
    [set looking-for-alternatives-tools True]

  ]
end
;--------------------END OF HUMAN THREAT OPERATIONALIZATION --------------------------------;


;; THREAT ON CROPS AND ELEPHANT ATTACK GO PROCEDURE
;-----------THRE
to simulate-hec ;threat operationalization

 simulate-attack-on-crops
  ;record-mitigation-damage-reduction
  record-mitigation-damage-reduciton-for-plant
  record-mitigation-effectivity

end

to simulate-attack-on-crops ;reassess for single adoption
  ask patches with [Land_type = "agriculture"] [set rng-visited-chance random-float 1]
  ask return-patch-set-to-be-attacked_v2  round (random-normal mean-attack std-attack) [
    set total-attacked total-attacked + 1
    set attacked? 1
    set pcolor red
    ;set pot-damage-before-mit 100 * (1 - age / 25)
    carefully [set pot-damage-before-mit 100 *  (e ^ (-.15 * age))][set pot-damage-before-mit 100] ;akan diupdate berdasarkan umur tanaman

      ]

  ;dari sini masukin ke module mitigation damage reduction
 ; dalam proses reduce ngikutin prosedur bertingkat dari record-mitigation-damage-reduction
  ; setelah direduce kasih damage level nya ke farmers yg memiliki lahan tersebut (scr average)
end

to record-mitigation-damage-reduciton-for-plant ;reassess for single adoption
  ask patches with [Land_Type = "agriculture"][

    let damage pot-damage-before-mit
    let reduced 0
    let residual 0


      ifelse mitigation-used != 0
      [let random-number random-float 1
      ifelse random-number <  actual_effectivity
      [set reduced pot-damage-before-mit ]
      [set reduced 0]
      ] ; ini yang akan diubah dengan basis cummulative distribution
      [set reduced 0  ]


    let l_tick_1 table:get mit_1-effect "timestep"
    set l_tick_1 lput ticks l_tick_1
    table:put mit_1-effect "timestep" l_tick_1

  ;-------------------------------------------------------------;
    let l_dmg_1 table:get mit_1-effect "damage"
    let l_reduced_1 table:get mit_1-effect "reduced"
    let l_resid_1 table:get mit_1-effect "residual"

  ;--------------------------------------------------------------;

    set l_dmg_1 lput damage l_dmg_1
    ifelse [attacked?] of self = 1
         [set l_reduced_1 lput reduced l_reduced_1]
         [set l_reduced_1 lput 0 l_reduced_1]

    set l_resid_1 lput (damage - reduced) l_resid_1

  ;--------------------------------------------------------------;

    table:put mit_1-effect "damage" l_dmg_1
    table:put mit_1-effect "reduced" l_reduced_1
    table:put mit_1-effect "residual" l_resid_1


    ;--------------------------------------------------------------;
 set damage-received-after-mit last l_resid_1  ; to assign the NET-DAMAGE-RECEIVED after mitigated
 ifelse crops-condition - damage-received-after-mit > 0
    [set crops-condition crops-condition - damage-received-after-mit]
    [set crops-condition 0 ]
    ; to assign the condition after being attacked and mitigated
  ]
end

to record-mitigation-effectivity ;reassess for single adoption
  ask farmers [

    let score_1 0
    ifelse any? land-owned with [attacked? = 1]
    [
      let denumerator count land-owned with [attacked? = 1 ]
      set score_1 count land-owned with [last table:get mit_1-effect "reduced" > 0] / denumerator
       ]
    [
      set score_1 0
      ;print scores
    ]
    ;putting the score values into each tables

    let eff_1 table:get eff-aux "a1-eff"
    set eff_1 lput score_1 eff_1
    table:put eff-aux "a1-eff" eff_1


    ]
 calculate-effectivity-mitigation-used
end

to calculate-effectivity-mitigation-used ;reassess for single adoption
  ask farmers [
    ;dropping all zeroes value
    py:set "var1" (table:get eff-aux "a1-eff")
    py:run "var1_aux = [x for x in var1 if x != 'NaN']"
    set effectivity-mit_1 py:runresult "sum (var1_aux) / len(var1_aux)"


  ]
end

to mitigation-bill
;note --> behaviour repurchasenya :

  ask farmers with [alternatives1_i != 0][

      ifelse capital_assets > ([operation_cost] of media (alternatives1_i - 1)) / 4
      [set capital_assets capital_assets - ([operation_cost] of media (alternatives1_i - 1) ) / 4

        (ask [land-owned] of self [
          set functionality 1
          set actual_effectivity [adv_effectivity] of media ([alternatives1_i] of myself - 1) ])
      ]
      [(ask land-owned [
        set actual_effectivity 0
        set functionality 2])]
    ]


  ;if pagar listrik --> cannot adopt another tools installation cost sekali; operation cost setiap 4 ticks (1 thn)
  ;if meriam --> setiap diserang --> installation dilakukan setiap diserang gajah ; baru bisa beli lagi setelah diserang
 ; if api unggun --> sama seperti meriam
end
;----------------END OF ELEPHANT ATTACK AND MITIGATION EFFECT OPERATIONALIZATION ----------------------;


;; CROPS FUNCTIONALITY AND HEC EFFECT ON CROPS;;;;;;;;;;;

to update-crops-attributes
  ask patches with [Land_Type = "agriculture"]
  [ set is-boundary-patches? update-boundaries-patch neighbors4
   ;(2) UPDATE AGE
   if ticks mod 4 = 0 and ticks != 0  and crops-ability-to-produce = 0 [ ;karena ticks dalam satuan triwulan maka age diupdate 4 ticks sekali
      set age age + 1 ]





   ;(3) RECORD HISTORICAL DATA
    record-historical-yield-on-crops
   ;(4) ASSESS CROP PRODUCTION CAPABILITY STATUS
    assess-crops-production-capability-status  ;revisit terkait routine sequence antara status assignment dan asset transfer
   ; (5) ASSETS TRANSFERS FROM CROPS TO FARMERS AND VICE VERSA
    capital-transfer-farmers-crops
   ;(1) ASESS TO REPURCHASE CROPS
    ;###SCENARIO I : IF CROPS YANG DITANAM SAMA DENGAN SEBELUMNYA
       repurchase-end-of-life-crops

    ;###SCENARIO II: JIKA ADA ROTASI CROPS (CROPS SETELAH TIDAK SAMA DENGAN YANG SEBELUMYA)

    ask ownership [set land-circumference assess-perimeter ownership]
  ]
end

to repurchase-end-of-life-crops
  if crops-ability-to-produce  = 1 [
    let name crop-type
     let replacement-expense item 0 [replacement-cost] of plantsDB with [commodity-name = name]
    ask ownership [
      ;reduce capital assets
      ifelse capital_assets - replacement-expense > 0
      [
        set capital_assets capital_assets - replacement-expense
        ask myself [
          set age 0 ;reset the age OPSI 2 : DISET AGE NYA DI AGE PRODUKTIF AJA
          set crops-ability-to-produce 0
        ]
      ]
      [
        ask myself [set crops-ability-to-produce 1 ]
      ]
    ]
  ]
end

to assess-crops-production-capability-status
  let name crop-type
  set crop-lifetime item 0 [crop-lifetime-data] of plantsDB with [commodity-name = name]
  let farmers-expense 0
  carefully [set farmers-expense last table:get historical-data  "cost"][set farmers-expense 0]

  ;(1) Kondisi I : Jika plant telah mencapai EOL
  if age  >= crop-lifetime [set crops-ability-to-produce 1 ]
  ;(2) Kondisi III : Jika petani bisa membayar dan belum mencapai EOL
  ;if age <= crop-lifetime and [capital_assets] of ownership > farmers-expense
  ;[set crops-ability-to-produce 0]
  ;(3) Kondisi II : Jika petani gagal melakukan perawatan
  ;if age <= crop-lifetime and [capital_assets] of ownership < farmers-expense [set crops-ability-to-produce 2]


end

to capital-transfer-farmers-crops
  ;;;PART 2 : ASSIGNING REVENUE AND EXPENSES KE FARMERS
 if ticks mod 4 = 0 [
  let farmers-income 0
   let farmers-expense 0

  ;(3) transfer kekayaan dari tanaman ketika panen
    ifelse crops-ability-to-produce = 0 [
    carefully [set farmers-income last table:get historical-data  "revenue"][set farmers-income 0]
    ask ownership [
      set capital_assets capital_assets + farmers-income
    ]][]
  ;(4) transfer kekayaan dari petani untuk biaya perawatan tanama
      carefully [set farmers-expense last table:get historical-data  "cost"][set farmers-expense 0]
    ifelse [capital_assets] of ownership - farmers-expense > 0
    [
      ask  ownership [set capital_assets capital_assets - farmers-expense]
      set crops-ability-to-produce 0
    ]
    [
      set crops-ability-to-produce 2
    ]
  ]

end

to record-historical-yield-on-crops
  ;NEED TO DO ;;:
 ; OPERATIONALIZATION DARI PERTUMBUHAN TANAMAN, JENIS TANAMAN, DAN PENGURANGAN KONDISI TANAMAN KETIKA DISERANG GAJAH
if ticks mod 4 = 0 [
    let name crop-type
    if crops-ability-to-produce = 0 [  ;apabila petani tidak bisa membayar di previous period maka tidak ada pertumbuhan or yield tanaman

    let previous-condition 100 ;innitiate aux var untuk ngerecord kondisi sebelumnya

    let crop-price item 0 [price] of plantsDB with [commodity-name = name]
      ;item 0 [price] of plantsDB with [commodity-name = crop-type]

   let umur table:get historical-data "year"
   let condition table:get historical-data  "condition"
   let cost table:get historical-data  "cost"
   let cum_cost table:get historical-data  "cum_cost"
   let yield table:get historical-data  "yield"
   let revenue table:get historical-data  "revenue"
   let lossess table:get historical-data  "opportunity_lossess"

      carefully [ set previous-condition last condition ] [set previous-condition [crops-condition] of self ] ;ngerecord kondisi sebelumnya

  ;(1) YEAR bisa ngambil dari variable age crops
    set umur lput [age] of self umur
  ;(2) CONDITION diambil dari variable condition patch
    set condition lput [crops-condition] of self condition
  ;(3) COST querry dari plantDB; tergantung jenis tanamannya dan dikaliin sama kondisinya (SINGLE QUERRY)
    set cost lput (single-querry-from-plantDB (name) (age) ("tc") * [crops-condition] of self / 100) cost
  ;(4) CUM_COST hasil penjumlahan seluruh data cost dari time 0
    set cum_cost lput sum cost cum_cost
  ;(5) YIELD diambil dari variable condition dikaliin dengan ideal yield di umur t berdasarkan querry table (make variable item berdasarkan prosedur position dari year)
    set yield lput (single-querry-from-plantDB name age "yield" * ([crops-condition] of self) / 100) yield
  ;(6) REVENUE perkalian dari YIELD dengan variable price plantDB
    set revenue lput (single-querry-from-plantDB name age "yield" * ([crops-condition] of self  / 100) * crop-price) revenue ;kurang prices
  ;(7) OPPORTUNITY_LOSSESS diambil dari perhitungan sesuai di ipad ; alternative : make prosedur slicing dari python (SLICING)
    set lossess lput (
        ( sum cost + slicing-querry-from-plantDB name age "yield") *
          ( (previous-condition - [crops-condition] of self) / 100) * crop-price
                                   ) lossess ;kurang price

    ;note : lossess perlu ditambahin formula (condition - damage) * yield (time t -> EOL) kemudian ditambahin cum cost
    ;note : formula no (7) perlu diadjust terhadap kondisi dia di tick sebelumnya dikurang dengan kondisi yang sekarang (bukan kondisi sempurna dikurang kondisi sekarang)
  table:put historical-data "year" umur
  table:put historical-data "condition" condition
  table:put historical-data "cost" cost
  table:put historical-data "cum_cost" cum_cost
  table:put historical-data "yield" yield
  table:put historical-data "revenue" revenue
  table:put historical-data "opportunity_lossess" lossess

    ]
 ]
end


;----------------END OF CROPS FUNCTIONALITY AND HEC EFFECT ON CROPS----------------------;
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;; SOCIAL NETWORK CONFIG
to top-down-communication-process

  ask mmedia
  [
    foreach sort farmers [ f ->
      let rn_p-reach random-normal top-down-success-reach-treshold .05
      let rn_p-success random-normal .2 .05

      if rn_p-reach > .8 [
        if rn_p-success > .2  [
          ask f [
            let k [product_advertised] of myself

            let j1 [adv_effectivity] of myself
            let j2 [adv_mitigationType] of myself
            let j3 [installment_cost] of myself
            let j4 [operation_cost] of myself
            let j5 [adv_product_support] of myself
            let j6 [access_to_credit] of myself



            ;info for Measurement efficacy
            set m-effectivity-info put-nested-table-lvl2 m-effectivity-info "media" k j1 ;effectivity

            ;info for Response Cost
            set r-installment_cost_info put-nested-table-lvl2 r-installment_cost_info  "media" k j3
            set r-operation_cost_info put-nested-table-lvl2 r-operation_cost_info "media" k j4

            ;info for Self efficacy
            set s-auxiliary-technical_dependency_i put-nested-table-lvl2 s-auxiliary-technical_dependency_i "media" k j5
            ;print j5
            table:put s-access-to-credit_i k j6

          ]

        ]

      ]
    ]
  ]
end

to export-ascii
;  ask patches with [ any? turtles-here ] [
 ;   set turtle-here 1
  ;]
  let ras_out gis:patch-dataset total-attacked
  gis:store-dataset ras_out "../Model_output/spatial_data/count_attacked.asc"

  let ras_out_2 gis:patch-dataset mitigation-used
  gis:store-dataset ras_out_2 "../Model_output/spatial_data/mitigation_used.asc"
end

;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################
;##############################################REPORT PROCEDURE###########################################################


;;@@@@@@@@@@@@@@@@@@@@ REPORT PROCEDURE FOR GO @@@@@@@@@@@@@@@@@@@@@@@@:
to-report calculate-installment-cost [circumference installment-cost]
  report installment-cost / 10 * circumference
end
to-report my-neighbor-EE  ;THREAT APPRAISALS
  let table table:make
  let att_list_dummy []
  let dmg_list_dummy []
  let pm_list_dummy []
      foreach sort (my_firstD_neigh)
       [neigh_i ->


      let a table:get [attacks-memory] of neigh_i "attacks"
      set att_list_dummy lput a att_list_dummy


      let d table:get [attacks-memory] of neigh_i "damaged?"
      set dmg_list_dummy lput d dmg_list_dummy

      let pm table:get [attacks-memory] of neigh_i "using-pm?"
      set pm_list_dummy lput pm pm_list_dummy ]

  ; end of foreach loop

  table:put table "attacks" flatten_list (flatten_list att_list_dummy)
  table:put table "damaged?" flatten_list (flatten_list dmg_list_dummy)
  table:put table "using-pm?" flatten_list (flatten_list pm_list_dummy)

    report table
end

to-report binary_assessment [alternative_used_list]  ;THREAT APPRAISALS
  let temp []
  foreach alternative_used_list [ k ->
    ifelse k = 0 [set temp lput 0 temp][set temp lput 1 temp]

  ]
  report temp
end

to-report operate-bnlearn [EE-tables]  ;THREAT APPRAISALS

  py:set "data" table:to-list EE-tables
  py:run "edges = [('attacks','damaged?'), ('using-pm?','damaged?')]"
  (py:run
    "def convert_netlogo_tables_to_df(netlogo_tables):"
    "     import pandas as pd"
    "     test_dik = {k[0]:k[1] for k in netlogo_tables}"
    "     test_dik"
    "     data_table=pd.DataFrame(test_dik, columns = test_dik.keys())"
    "     return data_table"

    "def inference_from_parameter_learning(df_obj, edges_obj):"
    "     import pandas as pd"
    "     import bnlearn as bn"

    "#creating BN network"
    "     DAG = bn.make_DAG(edges_obj,verbose=0)"
    "#fitting BN network w/ the data"
    "     model_mle= bn.parameter_learning.fit(DAG,df_obj,methodtype='bayes',verbose=0)"

    "#inferencing"
    "     try:"
    "          q1 = bn.inference.fit(model_mle, variables=['damaged?'], evidence={'attacks':1,'using-pm?':0},verbose=0)"
    "     except:"
    "          q1 = bn.inference.fit(model_mle, variables=['damaged?'], evidence={'attacks':1},verbose=0)"

    "#converting to dataframe"
    "     result=q1.df"
    "#returning P(DMG =1 | ATT = 1)"
    "     return  result['p'][1]"

  )
  (py:run
    "try:"
    "     result = inference_from_parameter_learning(convert_netlogo_tables_to_df(data),edges)"
    "except:"
    "     result = 0"
  )
  report py:runresult "result"
end

to-report bnlearn-comparison [EE-tables]  ;THREAT APPRAISALS
  py:set "data" table:to-list EE-tables

  (py:run
    "def convert_netlogo_tables_to_df(netlogo_tables):"
    "     import pandas as pd"
    "     test_dik = {k[0]:k[1] for k in netlogo_tables}"
    "     test_dik"
    "     data_table=pd.DataFrame(test_dik, columns = test_dik.keys())"
    "     return data_table"
  )

  (py:run
    "def calculate_inference(df):"
    "     try:"
    "          result = sum ( (df['damage']==1) & (df['attacks']==1) & (df['using-pm?']==0)) / sum((df['attacks']==1) & (df['using-pm?']==0))"
    "     except:"
    "          result = sum ( (df['damage']==1) & (df['attacks']==1)) / sum((df['attacks']==1))"
    "     return result"
   )
   report py:runresult "calculate_inference(convert_netlogo_tables_to_df(data))"

end

to-report select-one-of-info [v_neigh v_media]   ; FOR SELF EFFICACY
  py:set "a" v_neigh
  py:set "b" v_media

  (py:run
  "def select_one_of_value (neigh,media): #a is neighbor b is media"
  "  if neigh != 0:"
  "      if media!=0: "
  "          return neigh "
  "      else : "
  "          return neigh "
  "  elif media !=0 : "
  "      return media "
  "  else : "
  "      return 0")

  report py:runresult "select_one_of_value(a,b)"
end

to-report return_highest_value_from_table [table]  ;USED FOR BC ASSESSMENT
let product_key 1
       let highest_value 0
      let highest_key 0
  while [product_key <= product-available] [

      let candidate table:get table product_key

    if candidate > highest_value
    [set highest_value candidate set highest_key product_key]

    set product_key product_key + 1


    ]
  report highest_key
end

to-report return_descending_table_based_on_values [table]  ;USED FOR BC ASSESSMENT
  carefully [py:set "to_be_sorted_list" table:to-list table][py:set "to_be_sorted_list" table]
  report py:runresult "sorted(to_be_sorted_list, key=lambda x: x[1], reverse = True)"
end

to-report constrained_effectiveness_assessment [nested-list capital]
  py:set "candidates" nested-list
  py:set "capital" capital
  (py:run
    "def max_effectiveness(capital, candidates):"
    "     filtered_candidates = [c for c in candidates if c[2] <= capital]"
    "     if not filtered_candidates:"
    "        return (0,0)"
    "     max_candidate = max(filtered_candidates, key=lambda x: x[1])"
    "     return  max_candidate[0], max_candidate[2]"
    )
  report py:runresult "max_effectiveness(capital, candidates)"
end
to-report single-querry-from-plantDB [jenis-tanaman umur atribute-to-querry] ; USED FOR PLANT OPERATIONALIZATION single querry
  let table-fetch item 0 [commodity-data-table] of plantsDB with [commodity-name = jenis-tanaman];if jenis tanaman merupakan nama suatu tanaman


  ;let table-fetch [commodity-data-table] of plantDB (jenis-tanaman + product-available) ;if jenis tanaman number
  let list-attribute-fetch table:get table-fetch atribute-to-querry
  let item-attribute-fetch item (umur) list-attribute-fetch

  report item-attribute-fetch

end

to-report slicing-querry-from-plantDB [jenis-tanaman umur atribute-to-querry]
  let table-fetch item 0 [commodity-data-table] of plantsDB with [commodity-name = jenis-tanaman];if jenis tanaman merupakan nama suatu tanaman

  ;let table-fetch [commodity-data-table] of plantDB (jenis-tanaman + product-available) ;if jenis tanaman number
  let list-attribute-fetch table:get table-fetch atribute-to-querry

  py:set "slicing_querry" list-attribute-fetch
  py:set "age_when_attacked" umur
  report py:runresult "sum(slicing_querry[age_when_attacked+1::])" ;assuming panen terlebih dahulu sebelum diserang disaat timestep tersebut
end

to-report update-boundaries-patch [neigh-patch-set]
  report any? neigh-patch-set with [land_type != "agriculture" or crops-condition = 0]
end


to-report return-patch-set-to-be-attacked_v2  [number-to-be-called]
  let k number-to-be-called
  if k < 0 [set k 0]
  let r max-n-of k patches with [is-boundary-patches? = True] [ (visited-probability-from-raster-data - rng-visited-chance)]
  report  r
end

to-report return-patch-set-to-be-attacked
  let patch-sets []
  let rng-difference-generated-sets []
  foreach (list patches with [is-boundary-patches? = True])
  [bp -> set patch-sets lput [(list pxcor pycor)] of bp patch-sets
    ask bp [set rng-difference-generated-sets lput  (visited-probability-from-raster-data - rng-visited-chance) rng-difference-generated-sets ] ]

  report sort-list (item 0 patch-sets) rng-difference-generated-sets

  ;sort-list patch-sets rng-difference-generated-sets

end

to-report sort-list [list1 list2]
  let sorted-list map first sort-by [[item1 item2] -> last item1 > last item2] (map list list1 list2)
  report sorted-list
end

;;@@@@@@@@@@@@@@@@@@@@ REPORT PROCEDURE FOR INNITIALIZATION @@@@@@@@@@@@@@@@@@@@@@@@:

to-report innitiate-commodity-historical-yield-table
  let commodity-table table:make

  table:put commodity-table "year" []
  table:put commodity-table "condition" []
  table:put commodity-table "cost" []
  table:put commodity-table "cum_cost" []
  table:put commodity-table "yield" []
  table:put commodity-table "revenue" []
  table:put commodity-table "opportunity_lossess" []

  report commodity-table
end
to-report innitiate-damage-reduction-mit-aux
    let damage-record table:make


  table:put damage-record "timestep" []
  table:put damage-record "damage" []
  table:put damage-record "reduced" []
  table:put damage-record "residual" []

  report  damage-record



end

to-report innitiate-effectivity-probability-mitigation-list
  let effectiveness-probability table:make
  table:put effectiveness-probability "a1-eff" []


  report effectiveness-probability
end

to-report generate-level_1-product-table [random-input int-or-not?]
  let product_key 1
      let table_i table:make
      while [product_key <= product-available][

        ifelse int-or-not? = False
          [let k random-float random-input
              table:put table_i product_key k]
          [let k random random-input
               table:put table_i product_key k]

        set product_key product_key + 1
  ]

  report table_i
end

to-report innitiate-master-table-product-info
  let product_key 1
  let master_table table:make
  let from_neigh table:make
  let from_media table:make

  while [product_key <= product-available][
    table:put from_media product_key 0
    table:put from_neigh product_key 0


    set product_key product_key + 1
  ]
  table:put master_table "media" from_media
  table:put master_table "neigh" from_neigh

  report master_table

end

to-report count-product-availability [my_social_network]  ;revisit
  let product_key 1
  let table-product table:make

  while [product_key <= product-available][
    carefully
    [table:put table-product product_key count my_social_network with [alternatives1_i = product_key] ]
    [table:put table-product product_key 0]




    set product_key product_key + 1
  ]

  report table-product
end


;;@@@@@@@@@@@@@@@@@@@@ REPORT PROCEDURE FOR DATA HANDLING @@@@@@@@@@@@@@@@@@@@@@@@:

to-report value-mapper [values data-type]
  let results 0
  ;yang perlu dinormalisasi :
  ;TA
  ; (1) t-perceived-economical-lossess         ;currently continous number
  ;     t-crops-susceptibility
  ;     t-epidemical-evidence

  ;S-eff
  ; (2)  s-auxiliary-technical_capabilities_i  ;currently ordinal value from (1-4)
  ; (3)  s-auxiliary-technical_dependency_i    ;currently ordinal value from (1-4)
  ; (4)  s-access-to-credit_i                  ;currently ordinal value from (1-4)
  ; (5)  s-product_awareness_i                 ;currently number of neighbor adopting corresponding mit tools
  ; (6)  s-income-category                     ;currently ordinal value from (1-4)
  ; (7)  s-product_knowledge_i                 ;currently ordinal value from (1-4)
  ;M-eff
  ; (8)  m-effectivity-info                    ;currently [0,1]
  ;R-cost_i
  ; (9)  r-installment_cost_info               ;currently continous number
  ; (10) r-operation_cost_info                 ;currently continous number

 ;operasionalisasinya dilihat data typenya kemudian menggunakan if statement untuk setiap tipe data yang berbeda
  let d data-type
  let mapper [0 .25 .75 1]
  ;----------------------------------------------------BINS DEFINING FOR EACH VARIABLE HERE--------------------------------------------;
  ;threat var
  let t-perceived-economical-lossess_bins [0 1000000 3000000 60000000 90000000000 ]  ;dari penelitiannya mas restu terkait rata-rata kerugian mean -> 40.000.000 std -> 84 jt
  let t-crops-susceptibility_bins [0 .75 .8 .9 1]
  let t-epidemical-evidence_bins [0 .25 .5 .75 1]
  ;measurement efficacy var
  let m-effectivity-info_bins [0 .25 .5 .75 1]
  ;response cost var
  let r-installment_cost_info_bins [0 500000 1000000 5000000 50000000]
  let r-operation_cost_info_bins [0 100000 500000 1000000 2000000 ]
  ;self efficacy var
  let s-technical_dependency_bins [1 2 3 4 5]
  let s-auxiliary-technical_capabilities_bins [1 2 3 4 5]
  let s-auxiliary-technical_dependency_bins [1 2 3 4 5]
  let s-access-to-credit_bins  [1 2 3 4 5]
  let s-product_awareness_bins (list 0 (average-node-degree / (4 * 2)) (average-node-degree / (2 * 2)) average-node-degree  10000)
  let s-product_knowledge_bins [0 10 80 100 1000]

  ;----------------------------------------------------BINS DEFINING FOR EACH VARIABLE HERE--------------------------------------------;
  ;function that will be used in each if statements
  (py:run
     "import numpy as np"
     "def map_value(value, bins, mapper):"
         "     bins_copied = bins.copy()"
         "     bins_copied[-1] = np.inf"             ;changing the last boundaries to np.inf
         "     mapped_value = np.digitize(value, bins_copied, right=False)"
         "     #print(f'mapped_value: {mapped_value}')"
         "     result = mapper[mapped_value-1]"
         "     return  result "
       )
  ;ex IF data-type = "installment-cost" THEN  [normalizer]
  ;----------------------------------------------------MAPPER FOR EACH VARIABLE HERE--------------------------------------------;
  ;############################################THREAT VARIABLE###################################################################;
  if d = "t-perceived-economical-lossess"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" t-perceived-economical-lossess_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

   if d = "t-crops-susceptibility"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used"  t-crops-susceptibility_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

   if d = "t-epidemical-evidence"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" t-epidemical-evidence_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]


 ;############################################MEASUREMENT EFF VARIABLE#############################################################;
    if d = "m-effectivity-info"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" m-effectivity-info_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

 ;;############################################RESPONSE COST VARIABLE###############################################################;
    if d = "r-installment_cost_info"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" r-installment_cost_info_bins

    ;print (word "value: " values ", bins_used: " r-installment_cost_info_bins ", mapper: " mapper)
    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

    if d = "r-operation_cost_info"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" r-operation_cost_info_bins



    set results py:runresult "map_value(value, bins_used, mapper)"
  ]
 ;############################################SELF EFFICACY VARIABLE###################################################################;

   if d = "s-auxiliary-technical_capabilities_i"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" s-auxiliary-technical_capabilities_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

  if d = "s-auxiliary-technical_dependency_i"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" s-auxiliary-technical_dependency_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

  if d = "s-access-to-credit_i"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" s-access-to-credit_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

  if d = "s-product_awareness_i "
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" s-product_awareness_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

  if d = "s-product_knowledge_i"
  [
    py:set "value" values
    py:set "mapper" mapper
    py:set "bins_used" s-product_knowledge_bins

    set results py:runresult "map_value(value, bins_used, mapper)"
  ]

  report results
end

to refresh-visuals-and-state-var
  ;globals
  set cum-crop-raid-incident cum-crop-raid-incident + count patches with [attacked? = 1]

  ;patch related
    ;border-patch?
  ask patches with [land_type = "agriculture"]
  [set attacked? 0
    set pcolor green
   set pot-damage-before-mit 0
   set damage-received-after-mit 0
  ]

  ask patches with [land_type = "agriculture" and crops-condition = 0][set pcolor grey]
  ;farmers related
end

to-report get-nested-table [master-table source product_i] ; single value
  let master table:get master-table source
  let child table:get master product_i
  report child

end


to-report put-nested-table-lvl2 [master-table source_of_info product_i data]
    let master master-table
    let child table:get master source_of_info
    table:put child product_i data
    table:put master source_of_info child
  report master
end


to-report put-nested-table-lvl1 [master-table source_of_info child_table]
  let master master-table
  table:put master source_of_info child_table
  report master
end

to-report flatten_list [n-dimension-list]
py:set "a" n-dimension-list
  report py:runresult "[item for sublist in a for item in sublist]"
end

to-report assess-perimeter [current-turtle]
  let temp-per-len 0
  let border-patches patches with [ ownership = current-turtle and any? neighbors4 with [ ownership != current-turtle ] ]
  ask border-patches [
    let nobodies 4 - count neighbors4
    let non-territory-edges count neighbors4 with [ ownership != current-turtle ]
    let border-edges nobodies + non-territory-edges
    set temp-per-len temp-per-len + border-edges
  ]
  report temp-per-len
end

;#################################################### TO REPORT TRY PERFORM SA IN NETLOGO #########################################################;
to-report morris-params-gen
  py:setup py:python
  py:run "from SALib.sample.morris import sample as m_sample"
  py:run "from SALib.analyze.morris import analyze as m_analyze"
  py:run "import pandas as pd"

  (py:run
    "def morris_sample_gen(ns=2,num_level=4):"
    "     problem = {"

        "'names': ['random-seed',"
                  "'attacks-memory-len',"
                  "'average-node-degree',"
                  "'between-adoption-time-delay',"
                  "'avg-wealth',"
                  "'std-wealth',"
                  "'mean-attack',"
                  "'std-attack',"
                  "'top-down-success-reach-treshold',],"
        "'num_vars': 9,"
        "'bounds': [[1., 10000.],"
                   "[10., 50.],"
                   "[3., 15.],"
                   "[0.,12.],"
                   "[10000000.,900000000.],"
                   "[500000.,2000000],"
                   "[10,100],"
                   "[10,15],"
                   "[.1,.9],]}"

    "     n = ns"
    "     param_values = m_sample(problem, n, num_levels = num_level, local_optimization=True)"
    "     params_csv=pd.DataFrame(param_values,columns = problem['names'])"
    "     params_csv.to_csv('../Model_output/sensitivity_analysis/morris_params_csv.csv')"
    "     return param_values" )
  report (py:runresult "morris_sample_gen(ns=16,num_level=4)")


end
;####################################################END OF TO REPORT TRY PERFORM SA IN NETLOGO #########################################################;
@#$#@#$#@
GRAPHICS-WINDOW
205
54
715
385
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
-125
125
-80
80
0
0
1
ticks
30.0

BUTTON
8
10
71
43
NIL
setup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
9
52
72
85
NIL
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
2
364
175
397
between-adoption-time-delay
between-adoption-time-delay
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
0
238
172
271
average-node-degree
average-node-degree
0
100
8.0
1
1
NIL
HORIZONTAL

SLIDER
2
200
174
233
farmers-population
farmers-population
0
400
301.0
1
1
NIL
HORIZONTAL

SLIDER
3
93
180
126
trustworthiness-treshold
trustworthiness-treshold
0
1
0.25
.01
1
NIL
HORIZONTAL

TEXTBOX
10
174
160
193
Population Config\n
15
104.0
1

TEXTBOX
4
339
154
357
# Adoption Config
10
0.0
1

BUTTON
73
53
136
86
NIL
go
T
1
T
OBSERVER
NIL
F
NIL
NIL
1

PLOT
724
206
924
356
adoption-readiness
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"1" 1.0 0 -16777216 true "" "plot count farmers with [table:get adoption_readiness_i 1 = 1\n] "
"2" 1.0 0 -7500403 true "" "plot count farmers with [table:get adoption_readiness_i 2 = 1\n] "
"3" 1.0 0 -2674135 true "" "plot count farmers with [table:get adoption_readiness_i 3 = 1]"

SLIDER
8
131
180
164
attacks-memory-len
attacks-memory-len
0
100
40.0
1
1
NIL
HORIZONTAL

PLOT
723
362
923
512
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"no Adopt" 1.0 0 -4528153 true "" "plot count farmers with [alternatives1_i = 0]"
"1" 1.0 0 -16777216 true "" "plot count farmers with [alternatives1_i = 1]"
"2" 1.0 0 -955883 true "" "plot count farmers with [alternatives1_i = 2]"
"3" 1.0 0 -6459832 true "" "plot count farmers with [alternatives1_i = 3]"
"total" 1.0 0 -2674135 true "" "plot count farmers with [alternatives1_i != 0]"

PLOT
932
206
1135
354
capital assets of farmers
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot sum [capital_assets] of farmers  "

PLOT
934
362
1134
512
economic lossess
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot \nmean [t-perceived-economical-lossess] of farmers"
"sawit" 1.0 0 -12345184 true "" "plot \nmean [t-perceived-economical-lossess] of farmers with [[crop-type] of one-of land-owned = \"sawit\"]"
"karet" 1.0 0 -2674135 true "" "plot \nmean [t-perceived-economical-lossess] of farmers with [[crop-type] of one-of land-owned = \"karet\"]"

MONITOR
832
32
926
77
search market
count farmers with [looking-for-alternatives-tools = True] / count farmers
17
1
11

PLOT
933
49
1133
199
attacked patch
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot count patches with [attacked? = 1]\n"
"cummulative" 1.0 0 -2674135 true "" "plot cum-crop-raid-incident"

CHOOSER
41
522
179
567
scenario
scenario
"Baseline"
0

SLIDER
1311
27
1483
60
TA1
TA1
0
1
0.3
.001
1
NIL
HORIZONTAL

TEXTBOX
1311
10
1461
28
Parameter Config
10
0.0
1

SLIDER
1309
63
1481
96
TA2
TA2
0
1
0.3
.001
1
NIL
HORIZONTAL

SLIDER
1310
98
1482
131
TA3
TA3
0
1
0.3
.01
1
NIL
HORIZONTAL

SLIDER
1311
159
1483
192
SE1
SE1
0
1
0.3
.01
1
NIL
HORIZONTAL

SLIDER
1312
196
1484
229
SE2
SE2
0
1
0.3
.01
1
NIL
HORIZONTAL

SLIDER
1313
232
1485
265
SE3
SE3
0
1
0.3
.01
1
NIL
HORIZONTAL

SLIDER
1313
293
1485
326
RC1
RC1
-1
1
-0.5
.01
1
NIL
HORIZONTAL

SLIDER
1314
327
1486
360
RC2
RC2
-1
1
-0.5
.01
1
NIL
HORIZONTAL

PLOT
726
81
926
201
Threat-appraisals
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [threat-appraisals] of farmers"
"pen-1" 1.0 0 -2674135 true "" "plot mean [t-epidemical-evidence] of farmers"
"pen-2" 1.0 0 -7500403 true "" "plot count farmers with [looking-for-alternatives-tools = True] / count farmers"

TEXTBOX
12
418
162
436
Innitial Wealth Config
10
0.0
1

SLIDER
5
434
177
467
avg-wealth
avg-wealth
0
1000000000
4.0E7
100000
1
Rp
HORIZONTAL

SLIDER
5
469
177
502
std-wealth
std-wealth
0
300000000
1000000.0
50000
1
Rp
HORIZONTAL

TEXTBOX
237
417
387
435
# Elephant Attack Config
10
0.0
1

SLIDER
219
433
391
466
mean-attack
mean-attack
0
90
10.0
1
1
NIL
HORIZONTAL

SLIDER
218
469
390
502
std-attack
std-attack
0
20
2.0
1
1
NIL
HORIZONTAL

PLOT
1136
361
1392
512
plot 2
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"1-sawit" 1.0 0 -7500403 true "" "plot count farmers with [alternatives1_i = 0 and [crop-type] of one-of land-owned = \"sawit\"]"
"2-sawit" 1.0 0 -2674135 true "" "plot count farmers with [alternatives1_i = 2 and [crop-type] of one-of land-owned = \"sawit\"]"
"3-sawit" 1.0 0 -955883 true "" "plot count farmers with [alternatives1_i = 3 and [crop-type] of one-of land-owned = \"sawit\"]"
"1-karet" 1.0 0 -6459832 true "" "plot count farmers with [alternatives1_i = 1 and [crop-type] of one-of land-owned = \"karet\"]"
"2-karet" 1.0 0 -1184463 true "" "plot count farmers with [alternatives1_i = 2 and [crop-type] of one-of land-owned = \"karet\"]"
"3-karet" 1.0 0 -10899396 true "" "plot count farmers with [alternatives1_i = 3 and [crop-type] of one-of land-owned = \"karet\"]"
"no-sawit" 1.0 0 -13840069 true "" "plot count farmers with [alternatives1_i = 0 and [crop-type] of one-of land-owned = \"sawit\"]"
"no-karet" 1.0 0 -14835848 true "" "plot count farmers with [alternatives1_i = 0 and [crop-type] of one-of land-owned = \"karet\"]"

PLOT
513
390
713
540
plants 
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"age" 1.0 0 -16777216 true "" "plot mean [age] of patches with [land_type = \"agriculture\"] / 25"
"unproductive crops" 1.0 0 -2674135 true "" "plot count patches with [land_type = \"agriculture\" and crops-ability-to-produce = 2] / count patches with [land_type = \"agriculture\"]"

PLOT
724
516
924
666
Mitigation Functionality
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"1" 1.0 0 -12345184 true "" "plot count patches with [land_type = \"agriculture\" and mitigation-used = 1 and functionality = 1] / count patches with [land_type = \"agriculture\" and mitigation-used = 1 ] "
"2" 1.0 0 -7500403 true "" "plot count patches with [land_type = \"agriculture\" and mitigation-used = 2 and functionality = 1] / count patches with [land_type = \"agriculture\" and mitigation-used = 2 ] "
"3" 1.0 0 -2674135 true "" "plot count patches with [land_type = \"agriculture\" and mitigation-used = 3 and functionality = 1] / count patches with [land_type = \"agriculture\" and mitigation-used = 3 ] "
"all" 1.0 0 -955883 true "" "plot count patches with [land_type = \"agriculture\" and functionality = 1] / count patches with [land_type = \"agriculture\" ] "
"pen-4" 1.0 0 -6459832 true "" "plot mean [crops-condition] of patches with [land_type = \"agriculture\"] / 100 "

SLIDER
0
277
175
310
top-down-success-reach-treshold
top-down-success-reach-treshold
0
1
0.62
.01
1
NIL
HORIZONTAL

MONITOR
1139
62
1196
107
eff
mean [effectivity-mit_1] of farmers
3
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## CURRENT PROBLEM IN THIS VERSION 
#### crops expense is imbalance with crop revenue (SOLVED)
#### mitigation behaviour (repurchase, operation cost) needs to be assess 
#### self efficacy needs to be reassessed (SOLVED)
#### create a time gap between adoption window so that they dont readopt to frequently
#### When Adopting a tools Update the Effectivity, Installemnt Cost, and Operation Cost of the used products
#### add non protective response
non protective response happens when threat appraisals high and coping appraisals low
#### capital assets masih negative 
## Model Output 
  create an average crop lossess model based on Each mitigation type used 

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

# Media Entity 

Regarding the CA (Coping Appraisals) there is a media entity with available product knowledge that modeled in this simulation. The media entity will difffuse the information regarding a spesific product namely Reliability, Maintenance Effort, Availability/Reliability. The information spreading process will be modeled using Haer et. al Top Down approach. 

# Horizontal Information Spreading 

The information received by the agents will be spreaded throughout their network 



## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

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
NetLogo 6.3.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Baseline Condition" repetitions="20" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>mean [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [t-perceived-economical-lossess] of farmers</metric>
    <metric>count patches with [attacked? = 1]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-wealth">
      <value value="90000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="10000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Capital Assets Variation std and mean" repetitions="7" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>mean [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [t-perceived-economical-lossess] of farmers</metric>
    <metric>count patches with [attacked? = 1]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="avg-wealth" first="10000000" step="30000000" last="90000000"/>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Capital Assets Exploration" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>mean [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [t-perceived-economical-lossess] of farmers</metric>
    <metric>count patches with [attacked? = 1]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="avg-wealth" first="10000000" step="40000000" last="90000000"/>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Capital Assets Exploration V2" repetitions="5" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>mean [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [crops-condition] of patches with [land_type = "agriculture"]</metric>
    <metric>(count patches with [land_type = "agriculture" and crops-ability-to-produce = 2]) /( count patches with [land_type = "agriculture"])</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <steppedValueSet variable="avg-wealth" first="10000000" step="40000000" last="160000000"/>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Capital Assets Exploration V4" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [crops-condition] of patches with [land_type = "agriculture"]</metric>
    <metric>(count patches with [land_type = "agriculture" and crops-ability-to-produce = 2]) /( count patches with [land_type = "agriculture"])</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 1 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 1 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 2 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 2 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 3 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 3 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and functionality = 1] / count patches with [land_type = "agriculture" ]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-wealth">
      <value value="10000000"/>
      <value value="100000000"/>
      <value value="200000000"/>
      <value value="300000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Capital Assets Exploration V5" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [crops-condition] of patches with [land_type = "agriculture"]</metric>
    <metric>(count patches with [land_type = "agriculture" and crops-ability-to-produce = 2]) /( count patches with [land_type = "agriculture"])</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 1 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 1 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 2 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 2 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 3 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 3 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and functionality = 1] / count patches with [land_type = "agriculture" ]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-wealth">
      <value value="10000000"/>
      <value value="100000000"/>
      <value value="200000000"/>
      <value value="300000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Capital Assets Exploration V6" repetitions="10" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="80"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [crops-condition] of patches with [land_type = "agriculture"]</metric>
    <metric>(count patches with [land_type = "agriculture" and crops-ability-to-produce = 2]) /( count patches with [land_type = "agriculture"])</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 1 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 1 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 2 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 2 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 3 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 3 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and functionality = 1] / count patches with [land_type = "agriculture" ]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="trustworthiness-treshold">
      <value value="0.25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC1">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-wealth">
      <value value="200000000"/>
      <value value="300000000"/>
      <value value="400000000"/>
      <value value="500000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="RC2">
      <value value="-0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA1">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA2">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-wealth">
      <value value="1000000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="TA3">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="attacks-memory-len">
      <value value="40"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="std-attack">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="scenario">
      <value value="&quot;Baseline&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mean-attack">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="SE1">
      <value value="0.3"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Baseline Scenario Extended" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="20"/>
    <metric>mean [threat-appraisals] of farmers</metric>
    <metric>mean [t-epidemical-evidence] of farmers</metric>
    <metric>count farmers with [looking-for-alternatives-tools = True] / count farmers</metric>
    <metric>count farmers with [alternatives1_i = 1]</metric>
    <metric>count farmers with [alternatives1_i = 2]</metric>
    <metric>count farmers with [alternatives1_i = 3]</metric>
    <metric>count farmers with [alternatives1_i = 0]</metric>
    <metric>sum [capital_assets] of farmers</metric>
    <metric>sum [t-perceived-economical-lossess] of farmers</metric>
    <metric>mean [crops-condition] of patches with [land_type = "agriculture"]</metric>
    <metric>(count patches with [land_type = "agriculture" and crops-ability-to-produce = 2]) /( count patches with [land_type = "agriculture"])</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 1 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 1 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 2 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 2 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and mitigation-used = 3 and functionality = 1] / (count patches with [land_type = "agriculture" and mitigation-used = 3 ] + 1)</metric>
    <metric>count patches with [land_type = "agriculture" and functionality = 1] / count patches with [land_type = "agriculture" ]</metric>
    <enumeratedValueSet variable="between-adoption-time-delay">
      <value value="4"/>
      <value value="4"/>
      <value value="4"/>
      <value value="12"/>
      <value value="12"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="farmers-population">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="avg-wealth">
      <value value="900000000"/>
      <value value="900000000"/>
      <value value="10000000"/>
      <value value="10000000"/>
      <value value="10000000"/>
      <value value="10000000"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
