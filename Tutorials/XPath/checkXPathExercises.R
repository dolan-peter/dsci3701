library(htmltools)
library(xml2)
library(data.tree)
#Need some tools to make a table  Looks like I could use tagAppendChild and or tagAppendChildren

td=function(cells){lapply(cells,function(cell){tag("td",cell)})}
tr=function(...){tag("tr",...)} #Wraps  LIST (or vector) of TD's into one TR
html.table=function(...){tag("table",...)} #wraps a LIST (or vector) of TR's into one table

safeTry=function(expr){
  results=tryCatch({
    expr=substitute(expr) #Reparse expr as an expression-- this can throw an error too.
    if(length(expr)>0){ #If there is anything to evaluate... then do so.
      eval.parent(expr)
    }
    },warning=function(e){FALSE},error=function(e){FALSE})
    if(length(results)==0){return(FALSE)}
  return(results)
}
quest=function(){
  full.points=0
  score=0
  correct.text=""
  incorrect.text=""
  et=as.factor(c("correct","partial","wrong"))
  result=et[3] #Set to wrong
  
  setValue=function(n){full.points<<-n}
  setResults=function(n){points<<-n} # add check that points is <=value
  setCorrect=function(n=NULL){
    result<<-et[1]
    if(is.null(n)){score<<-full.points}
    else {score<<-n}
  }
  setPartial=function(n=NULL){
    result<<-et[2]
    if(is.null(n)){score<<-full.points/2}
    else{score<<-n}
  }
  setWrong=function(n=null){
    result<<-et[3]
    if(is.null(n)){score<<-0}
    else{score<<-n}
  }
  setText=function(correct="",incorrect=""){
    correct.text<<-correct
    incorrect.text<<-incorrect
  }
  getValue=function(){full.points}
  getResults=function(){score}
  getCorrect.text=function(){correct.text}
  getIncorrect.text=function(){incorrect.text}
  isNotRight=function(){result!="correct"}
  list(setValue=setValue,setResults=setResults,setCorrect=setCorrect,setPartial=setPartial,setWrong=setWrong,setText=setText,getValue=getValue,getResults=getResults,getCorrect.text=getCorrect.text,getIncorrect.text=getIncorrect.text,isNotRight=isNotRight)
}

problem=function(){
  questions=list()
  
  addQuestion=function(id="Question",correct="",incorrect="",pt.value=2){
    q=quest()
    q$setValue(pt.value)
    q$setText(correct,incorrect)
    questions[[id]]<<-q
  }
  
  getQuestions=function(){
    questions
  }
  setCorrect=function(id,n=NULL){
    q<-questions[[id]]
    q$setCorrect(n)
  } #add a setWrong too
  setPartial=function(id,n=NULL){
    q<-questions[[id]]
    q$setPartial(n)
  }
  testAll=function(){
    for(q in questions){
      if(q$isNotRight()){return(FALSE)}
    }
    return(TRUE)
  }
  totalScore=function(){
    score=0
    for(q in questions){
      score=score+q$getResults()
    }
    score
  }
  
  totalPoints=function(){
    total=0
    for(q in questions){
      total=total+q$getValue()
    }
    total
  }
  
  getIncorrectMessages=function(sep=":"){
    msg=""
    for(q in questions){
      if(q$isNotRight()){
        msg=paste0(msg,sep,q$getIncorrect.text())
      }
    }
    return(msg)
  }
  getFeedbackTable=function(){
    results=c()
    points=c()
    feedback=c()
    for(q in questions){
      points=c(points,paste0("[",q$getResults(),"] pts"))
      if(q$isNotRight()){
        results=c(results,"❌")
        feedback=c(feedback,q$getIncorrect.text())
      }else{
        results=c(results,"✅")
        feedback=c(feedback,q$getCorrect.text())
      }
    }
    feedback.table<-cbind(results,feedback,points)
    tmp=apply(feedback.table,1,paste)
    msg=paste(tmp,collapse=" ")
#    tmp<-apply(feedback.table,1,function(cells){tr(td(cells))})#Make the rows
    #return(html.table(tmp))
    return(msg)
  }
  return(list(
    testAll=testAll,
    setCorrect=setCorrect,
    setPartial=setPartial,
    getQuestions=getQuestions,
    addQuestion=addQuestion,
    totalScore=totalScore,
    totalPoints=totalPoints,
    getIncorrectMessages=getIncorrectMessages,
    getFeedbackTable=getFeedbackTable))
}

problemSet=function(){
  problems=list()
  
  addProblem=function(id=NULL,prob=NULL){
    if(is.null(id)){id=length(problems)+1}
    if(is.null(prob)){prob<-problem()}
    problems[[id]]<<-prob
  }
  
  getProblem=function(id){
    problems[[id]]
  }

  totalScore=function(){
    sum(sapply(problems,function(p){p$totalScore()}))
  }
  report=function(){
    
    scores=sapply(problems,function(p){p$totalScore()})
    scores=0
    for(p in problems){
      scores=p$totalScore()+scores
    }
    return(as.character(scores))
    return(paste0(scores,collapse="\n"))
    values=sapply(problems,function(p){p$totalPoints()})
    msg=paste0("[",names(problems),"]:",scores,"/",values,collapse="\n")
    msg=paste0(msg,"Total:",sum(scores),"/",sum(values),"\n")
    return(msg)
  }
  getProblems=function(){problems}
  return(list(addProblem=addProblem,totalScore=totalScore,report=report,getProblems=getProblems,getProblem=getProblem))
}

check_exercise <- function(
  label,
  user_code,
  solution_code,
  check_code,
  envir_result,
  evaluate_result,
  envir_prep,
  ...
) {
  
  safeTry(eval(parse(text=user_code)))
  safeTry(eval(parse(text=check_code))) #Expectation is that code will generate an `exercise` object
  
  #eval(parse(text=check_code)) #Expectation is that code will generate an `exercise` object
    
  if(!exists("exercise")){
    return(list(message="Fatal error in user code: aborting check.  Please rewrite code",correct=FALSE,location="append"))
  }
  msg=as.character(exercise$getFeedbackTable());
  score=exercise$totalScore()
  points=exercise$totalPoints()
  msg=paste0("(",score,"/",points," pts) ",msg)
  correct=exercise$testAll()
    list(message = msg, correct = correct, location = "append")
}

#ps.file="localdata.rdata" #Needs to be run on a local machine
ps=problemSet() #Setup the problem set... totally worthless right now.

makeDirList=function(){
  dirs<-list.dirs(recursive=TRUE)
  tmp<-strsplit(dirs,"/")
  dir.list=list("."=list())
  invisible(#build a list to match the tree structure of the directory
    sapply(tmp,
           function(sub.dir){
             path=c()
             for(sub in sub.dir){
               path=c(path,sub)
               if(is.null(dir.list[[path]])){dir.list[[path]]<<-list()}
             }
           })
  )
  return(dir.list)
}