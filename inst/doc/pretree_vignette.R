## ----setup, echo=FALSE,message=FALSE,warning=FALSE----------------------------
knitr::opts_chunk$set(collapse = T, comment = "#>")
options(tibble.print_min = 4L, tibble.print_max = 4L)
set.seed(123)
library(irtrees)
library(flextable)
library(reshape2)
library(mirt)
library(lme4)

## ---- echo=FALSE--------------------------------------------------------------
g = data.frame(person=c('John','Mary'),
               sex=c('male','female'),
               item1=c("A","A"),
               item2=c("C",'B'))
flextable(g)

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
flextable(melt(g,id.vars='person'))

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
flextable(melt(g,id.vars=c('person','sex')))

## ---- echo=FALSE, warning=FALSE-----------------------------------------------
flextable(melt(g[,-2],id.vars=c('person')))
flextable(g[,1:2])

## ---- echo=FALSE--------------------------------------------------------------
f=expand.grid(c('Wide','Long'),c('Wide','Long'),c('single','multiple'))
names(f)=c('Input','Output','Tree')
f$Function = paste0(substr(f$Input,1,1),'to',substr(f$Output,1,1),'_',f$Tree,'.tree')
f$Function = gsub('multiple','multi',f$Function)
autofit(flextable(f))

## -----------------------------------------------------------------------------
five_wide = data.frame(
  id = 1:100,
  gender = factor(sample(c('M','F'), 100, replace=TRUE)),
  item1 = sample.int(5, 100, replace = TRUE),
  item2 = sample.int(5, 100, replace = TRUE),
  item3 = sample.int(5, 100, replace = TRUE),
  item4 = sample.int(5, 100, replace = TRUE),
  item5 = sample.int(5, 100, replace = TRUE)
  )
str(five_wide)

## ---- echo=FALSE--------------------------------------------------------------
library(DiagrammeR)
grstr = "digraph linear {
            node [shape=oval]
              Y1; Y2; Y3; Y4
            node [shape=box]
              Neutral; 'Strongly disagree'; Disagree; Agree; 'Strongly agree'
            edge [label='1']
              Y1->Neutral; Y2->Y4; Y3->'Strongly disagree'; Y4->'Strongly agree'
            edge[label='0']
              Y1->Y2; Y2->Y3; Y3->Disagree; Y4->Agree;
          }"
tree=grViz(grstr)
tree

## ---- echo=F------------------------------------------------------------------
five_cmx = matrix(c(0,  0,  1,  0,  0,
                    0,  0,  NA, 1,	1,
                    1,  0,  NA, NA, NA, 
                    NA, NA, NA, 0,  1), nrow = 5)

cats = c('Strongly disagree', 'Disagree', 'Neither agree or disagree', 'Agree', 'Strongly Agree')
z=cbind(cats,as.data.frame(five_cmx))
names(z)=c(' ',paste0('Y',1:4))
flextable(z) |> colformat_num(na_str = "NA") |> bg(j=grep("Y", colnames(z), value = TRUE), bg='skyblue', part='body')

## -----------------------------------------------------------------------------
five_wide_single.tree = WtoW_single.tree(
  data = five_wide, 
  cmx = five_cmx, 
  id.col = 1, 
  resp.col = c(3:7), 
  covar.col = 2
  )
str(five_wide_single.tree)

## ---- eval=FALSE--------------------------------------------------------------
#  five_wide_single.tree = WtoW_single.tree(
#    data = five_wide,
#    cmx = five_cmx,
#    id.col = "id",
#    resp.col = c("item1", "item2", "item3"),
#    covar.col = "gender"
#  )

## -----------------------------------------------------------------------------
five_long_single.tree = WtoL_single.tree(
  data = five_wide, 
  cmx = five_cmx, 
  id.col = "id", 
  resp.col = c("item1","item2","item3"), 
  covar.col = "gender")

str(five_long_single.tree)

## ---- echo=FALSE--------------------------------------------------------------
five_cmx.2 = matrix(c(0,  0,  1,  0,  0,
                      0,  1, NA,  2,	3), nrow = 5)
five_cmx.2

## ---- eval=FALSE--------------------------------------------------------------
#  matrices_list = list(five_cmx, five_cmx.2)
#  
#  items_list = list(c('item1', 'item3', 'item5'), c('item2', 'item4'))
#  
#  five_wide_multiple.tree = WtoW_multiple.tree(
#    data = five_wide,
#    cmx_list = matrices_list,
#    resp.col_list = items_list,
#    id.col = "id",
#    covar.col = "gender"
#  )

## -----------------------------------------------------------------------------
VerbAgg$resp = as.integer(VerbAgg$resp)

## ---- echo=FALSE--------------------------------------------------------------
lintree = 'graph TB
  X1((X1))  --0-->  L1[no]
  X1((X1))  --1-->  X2((X2))
  X2((X2))  --0-->  L2[perhaps]
  X2((X2))  --1-->  L3[yes]'
DiagrammeR::mermaid(lintree)

lincmx = graph2mx(lintree)

cats = c('no', 'perhaps', 'yes')
z=cbind(cats,as.data.frame(lincmx))
names(z)=c(' ',paste0('X',1:2))
flextable(z) |> colformat_num(na_str = "NA") |> bg(j=grep("X", colnames(z), value = TRUE), bg='skyblue', part='body')

## -----------------------------------------------------------------------------
VerbAgg$resp = as.integer(VerbAgg$resp)

VerbAgg_wide = LtoW_single.tree(
  data = VerbAgg, 
  cmx = lincmx, 
  id.col = "id",
  item.col = "item", 
  resp.col = "resp", 
  covar.col = c("Anger","Gender","btype","situ","mode"))

str(VerbAgg_wide)

## -----------------------------------------------------------------------------
covdata = VerbAgg_wide[,2:3]
VerbAgg_wide = VerbAgg_wide[,-(1:3)]
itemdesign = data.frame(node = factor(rep(1:2, each=24)))
itemdesign$mode = factor(ifelse(grepl('Do', names(VerbAgg_wide)), 'Do', 'Want'))

## -----------------------------------------------------------------------------
VerbAgg_long = LtoL_single.tree(
  data = VerbAgg, 
  cmx = lincmx, 
  id.col = "id",
  item.col = "item", 
  resp.col = "resp", 
  covar.col = c("Anger","Gender","btype","situ","mode"))

str(VerbAgg_long)

## ---- eval=FALSE--------------------------------------------------------------
#  model1 = glmer(resp ~ 0 + node:item + (1 | id),        family=binomial, data=VerbAgg_long)
#  model2 = glmer(resp ~ 0 + node:item + (0 + node | id), family=binomial, data=VerbAgg_long)

## -----------------------------------------------------------------------------
model1 = glmer(resp ~ 0 + node + (0 + node | item) + (1 | id),        family=binomial, data=VerbAgg_long)
model2 = glmer(resp ~ 0 + node + (0 + node | item) + (0 + node | id), family=binomial, data=VerbAgg_long)

## -----------------------------------------------------------------------------
mm1 = "F1=1-48"

mm2 = "F1=1-24 
       F2=25-48 
       COV=F1*F2"

## ---- eval=FALSE--------------------------------------------------------------
#  mirt1 =  mirt(data = VerbAgg_wide, model = mm1, itemtype="Rasch", verbose=FALSE)
#  mirt2 =  mirt(data = VerbAgg_wide, model = mm2, itemtype="Rasch", verbose=FALSE)

## -----------------------------------------------------------------------------
mirt1 =  mixedmirt(data=VerbAgg_wide, model=mm1, fixed = ~ 0 + node, random= ~ 1 | items, 
                   itemdesign=itemdesign, SE=TRUE, verbose=FALSE)
mirt2 =  mixedmirt(data=VerbAgg_wide, model=mm2, fixed = ~ 0 + node, random= ~ 1 | items, 
                   itemdesign=itemdesign, SE=TRUE, verbose=FALSE)

## -----------------------------------------------------------------------------
anova(model1, model2)
anova(mirt1, mirt2)

## -----------------------------------------------------------------------------
mytree = 'graph TB
  X1  --0-->  no
  X1  --1-->  X2
  X2  --0-->  perhaps
  X2  --1-->  yes'

DiagrammeR::mermaid(mytree)

## -----------------------------------------------------------------------------
mytree = 'graph TB
  X1((X1))  --0-->  L1[no]
  X1((X1))  --1-->  X2((X2))
  X2((X2))  --0-->  L2[perhaps]
  X2((X2))  --1-->  L3[yes]'

## -----------------------------------------------------------------------------
mmx = graph2mx(mytree)
mmx

## -----------------------------------------------------------------------------
tree1 = 'graph TB
  X1((X1)) --1--> L1[Neutral]
  X1((X1)) --0--> X2((X2))
  X2((X2)) --0--> X3((X3))
  X2((X2)) --1--> X4((X4))
  X3((X3)) --1--> L2[Strongly disagree]
  X3((X3)) --0--> L3[Disagree]        
  X4((X4)) --0--> L4[Agree]
  X4((X4)) --1--> L5[Strongly agree]'

DiagrammeR::mermaid(tree1)

## -----------------------------------------------------------------------------
graph2mx(tree1)

## ---- echo=FALSE--------------------------------------------------------------
mytree = 'graph TD
  X1((X1)) --1--> L1[Neutral]
  X1((X1)) --0--> X2((X2))
  X2((X2)) --0--> X3((X3))
  X2((X2)) --1--> X3((X3))
  X3((X3)) --1--> L2[Strongly disagree]
  X3((X3)) --0--> L3[Disagree]        
  X3((X3)) --0--> L4[Agree]
  X3((X3)) --1--> L5[Strongly agree]'

DiagrammeR::mermaid(mytree)

## ---- echo=FALSE--------------------------------------------------------------
tree2 = 'graph TD
  X1((X1)) --1--> L1[Neutral]
  X1((X1)) --0--> X2((X2))
  X2((X2)) --0--> X3((X3))
  X2((X2)) --1--> X4((X3))
  X3((X3)) --1--> L2[Strongly disagree]
  X3((X3)) --0--> L3[Disagree]        
  X4((X3)) --0--> L4[Agree]
  X4((X3)) --1--> L5[Strongly agree]'

DiagrammeR::mermaid(tree2)

## -----------------------------------------------------------------------------
mmx = graph2mx(tree2)
mmx

