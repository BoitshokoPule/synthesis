module Synthesis

open System.Xml.Linq
open System.Xml.Linq
open System.Xml.Linq
open System.Security.Cryptography.X509Certificates

let abelar x =  x>12 && x<3097 && x%12=0
failwith "Not implemented"

let area b h = match b<0.0 ||h<0.0 with |true -> failwith "Either your base or height is a negative number." | _ ->1.0/2.0*h*b
failwith "Not implemented"

let zollo n = match n<0 with |true -> n*(-1) |_ -> n*2  
failwith "Not implemented"

let min x y = match x<y with | true -> x |_ -> y
failwith "Not implemented"

let max x y = match x<y with |true -> y |_ -> x
failwith "Not implemented"

let ofTime h m s = h*3600+m*60+s
failwith "Not implemented"

let toTime s = match s>0 with |true-> s/3600,(s%3600)/60,s%60 |_ -> (0,0,0) 
failwith "Not implemented"

let digits d =
              let origin= 10
              let rec dgt num acc=
                  match d/num=0 with 
                  |true -> acc 
                  |false -> dgt (num*10) (acc+1)
              dgt origin 1
failwith "Not implemented"

let minmax (b,o,i,t) = min(min b o) (min i t),max(max b o) (max i t)
failwith "Not implemented"

let isLeap x = 
         match x<1582 with 
         |true ->failwith "Increase the year"
         |false ->
                  match (x%4=0 && x%100<>0) || (x%400=0) with
                  |true -> true
                  |_ -> false
failwith "Not implemented"

let month =function
             |1 -> "January",31
             |2 -> "February",28
             |3 -> "March",31
             |4 -> "April",30
             |5 -> "May",31
             |6 -> "June",30
             |7 -> "July",31
             |8 -> "August",31
             |9 -> "September",30
             |10 -> "October",31
             |11 -> "November",30
             |12 -> "December",31
             |_ -> failwith "I am throwing an exception"
             
failwith "Not implemented"

let toBinary n =
                match n<0 with
                |true ->failwith "Supply a positive number"
                |false -> 
                         match n=0 with
                               |true -> "0"
                               |false ->
                               let rec binary x acc =
                                 match x=0 && acc<>"" with
                                 |true ->(acc)
                                 |false ->
                                       match x%2=0 with
                                       |true -> binary (x/2) ("0"+acc)
                                       |false -> binary (x/2) ("1"+acc)
                               binary n ""
failwith "Not implemented"

let bizFuzz n =
                let rec incnum x (acc1,acc2,acc3)=
                  match x<=n with
                  |true ->match x%3=0, x%5=0, (x%3=0 && x%5=0) with
                          |true,false,false -> incnum(x+1) (acc1+1,acc2,acc3)
                          |false, true,false -> incnum(x+1) (acc1,acc2+1,acc3)
                          |false,false, true -> incnum(x+1) (acc1,acc2,acc3+1)
                          |true,true,false -> incnum(x+1) (acc1+1,acc2+1,acc3)
                          |true,false, true -> incnum(x+1) (acc1+1,acc2,acc3+1)
                          |false,true, true -> incnum(x+1) (acc1,acc2+1,acc3+1)
                          |true, true, true -> incnum(x+1) (acc1+1,acc2+1,acc3+1)
                          |_,_,_ -> incnum(x+1) (acc1,acc2,acc3)
                  |false -> (acc1,acc2,acc3)
                incnum 1 (0,0,0)
failwith "Not implemented"

let monthDay days year =  
                     let leap,till=
                      match isLeap year with
                      |true -> true,366
                      |false -> false,365
                     match (days<=0),(days=366 && leap=false),(days>366) with
                     |true,false,_ ->failwith "I am throwing an exception"
                     |false,true,false ->failwith "I am throwing an exception"
                     |false,false,true -> failwith "I am throwing an exception"
                     |_,_,_ -> 
                              let rec checkamonth x num=
                                 match x=13 with
                                 |true -> "December"
                                 |false -> 
                                          let m,day=month x
                                          let ohtwocheck= 
                                           match x=2 && leap with
                                           |true -> day+1
                                           |false -> day
                                          match num< days with
                                          |true -> checkamonth (x+1) (num+ohtwocheck)
                                          |false ->let m,days= month (x-1)
                                                   m
                              checkamonth 1 0
failwith "Not implemented"
let sqrt num=
      let rec sqrtcal get j=
          match j with
          |10 -> get
          |_ -> let f=(get + num/get)/2.0
                sqrtcal f (j+1)
      match num<0.0 with
      |true -> failwith "Positive number please"
      |_ -> sqrtcal (num/2.0) 0
           
let coord n =
      let (x1,x2)= n
      let distance (x3,x4)= sqrt((x1-x3)**2.0 + (x2-x4)**2.0)
      let ishere (x,y) width height=
           match (x1<=(x+width) && x1 >=x) && (x2<=y && x2>=y-height) with
           |true ->true
           |_ -> false
      distance, ishere
failwith "Not implemented"