convertBinToDec(B,D):- atom_number(S,B),
                       atom_length(S,L),
                       sub_atom(S, 0, 1, After,S1),
					   atom_number(S1,N),
					   sub_atom(S, 1,After,0 ,S2),
					   atom_number(S2,B2),
                       convertBinToDec(B2,D1),
					   D is D1+((2*N)**(L-1)).
convertBinToDec(0,0).
convertBinToDec(1,1).

getNumBits(_,fullyAssoc,_,0).
getNumBits(NumOfSets,setAssoc,_,BitsNum):-logBase2(NumOfSets,BitsNum).
getNumBits(_,directMap,Cache,BitsNum):-length(Cache,R),
                                       logBase2(R,BitsNum).
replaceIthItem(X, [_|T], 0, [X|T]).
replaceIthItem(X, [H|T], I, [H|R]):- I > 0,
                                     I1 is I-1, 
							         replaceIthItem(X, T, I1, R).

logBase2(I,E):-
  (number(E),I is 2**E);
  (number(I),E is ceiling(log(I)/log(2))).
  
splitEvery(_,[],[]).
splitEvery(N,L,[H|T]):-length(H,N),
                       append(H,T1,L),
					   splitEvery(N,T1,T).
                        
fillZeros(S,0,S).
fillZeros(S,N,R):-N>0,
                  N1 is N-1,
				  fillZeros(S,N1,R1),
				  string_concat("0",R1,R).
				 				  				  

	
convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
	Idx is  Bin mod 10**BitsNum,
	BitsNum > 0,
	Tag is Bin // 10**BitsNum.


convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
	logBase2(SetsNum,N),
	Idx is Bin mod(10**N),
	Tag is Bin//(10**N).	

getDataFromCache(StringAddress,Cache,Data,0,directMap,BitsNum):-atom_number(StringAddress, X),
																convertAddress(X,BitsNum,Tag,Idx,directMap),
																convertBinToDec(Idx,Index),
																atom_number(StringTag, Tag),
																atom_length(StringAddress,Lt),
																atom_length(StringTag,L),
																F is (Lt-BitsNum-L),
																fillZeros(StringTag,F,Rt),
																nth0(Index,Cache,item(tag(Rt),data(Data),1,_)).
	  
getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
	length(Cache , L),
	SetSize is L//SetsNum,
	splitEvery(SetSize,Cache,NewCache),
	atom_number(StringAddress, N),
	convertAddress(N,SetsNum,Tag,Idx,setAssoc),
	convertBinToDec(Idx,Index),
	atom_number(StringTag, Tag),
	atom_length(StringAddress,Lt),
	logBase2(SetsNum,I),
	atom_length(StringTag,L1),
	Zero is Lt-I-L1,
	fillZeros(StringTag,Zero,StringTag1),
	nth0(Index	,NewCache,Needed_set),
	nth0(HopsNum,Needed_set,item(tag(StringTag1),data(Data),1,_)).

		
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-atom_number(Tag_s,Tag),
																		  atom_number(Idx_s,Idx),
																		  atom_length(Tag_s,L),
																		  X is (6-L-BitsNum),
																		  fillZeros(Tag_s,X,Rt),
																		  atom_length(Idx_s,L1),
																		  Y is BitsNum-L1,
																		  fillZeros(Idx_s,Y,Ri),
																		  string_concat(Rt,Ri,Address),
																		  atom_number(Address,Address_bin),
                                                                          convertBinToDec(Address_bin,Address_int_m),
                                                                          nth0(Address_int_m,Mem,ItemData),
																		  convertBinToDec(Idx,Address_c),
                                                                          replaceIthItem(item(tag(Rt),data(ItemData),1,0),OldCache,Address_c,NewCache).


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-       atom_number(Tag_s,Tag),
																		    atom_number(Idx_s,Idx),
																			atom_length(Tag_s,L),
																			X is 6-L,
																			fillZeros(Tag_s,X,Rt),
																		    string_concat(Tag_s,Idx_s,Address),
																		    atom_number(Address,Address_bin),
                                                                            convertBinToDec(Address_bin,Address_int_m),
                                                                            nth1(Address_int_m,Mem,ItemData), 
																			oldest_item(Y,OldCache),
																		    nth0(P,OldCache,Y),
																			increment_clock(OldCache,Z),
																			replaceIthItem(item(tag(Rt),data(ItemData),1,0),Z,P,NewCache).
																			


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):- atom_number(Tag_s,Tag),
																		  atom_number(Idx_s,Idx),
																		  atom_length(Tag_s,L),
																		  logBase2(SetsNum,L1),
																		  X is (6-L-L1),
																		  fillZeros(Tag_s,X,Rt),
																		  atom_length(Idx_s,L2),
																		  Q is L1-L2,
																		  fillZeros(Idx_s,Q,Ri),
																		  string_concat(Rt,Ri,Address),
																		  atom_number(Address,Address_bin),
                                                                          convertBinToDec(Address_bin,Address_int_m),
                                                                          nth0(Address_int_m,Mem,ItemData),
																		  length(OldCache , F),
																		  SetSize is F//SetsNum,
																		  splitEvery(SetSize,OldCache,Sets),
																		  fillZeros(Tag_s,X,Rt),
																		  convertBinToDec(Idx,Address_set),
																		  nth0(Address_set,Sets,Needed_set),
																		  oldest_item(Y,Needed_set),
																		  nth0(P,Needed_set,Y),
																		  increment_clock(Needed_set,Z),
																		  replaceIthItem(item(tag(Rt),data(ItemData),1,0),Z,P,Z1),
                                                                          replaceIthItem(Z1,Sets,Address_set,Z2),
																		  flatten(Z2,NewCache).




 # older(item(_,_,0,Order),item(_,_,0,Order1)):-Order>Order1. that will get the oldest invalid not the first invalid
older(item(_,_,0,_),item(_,_,0,_)). 
older(item(_,_,0,_),item(_,_,1,_)).
older(item(_,_,1,Order),item(_,_,1,Order1)):-Order>Order1.

oldest_item(item(A,B,0,C),[item(A,B,0,C)|_]).			
oldest_item(H,[H]).													
oldest_item(H,[H|T]):-oldest_item(R,T),
                      older(H,R).	
oldest_item(R,[H|T]):-oldest_item(R,T),
                      older(R,H).	
					  

increment_clock([],[]).
increment_clock([item(A,B,1,Order)|T],[item(A,B,1,Order1)|T2]) :- Order1 is Order+1,
                                                                  increment_clock(T,T2).

increment_clock([item(A,B,0,Order)|T],[item(A,B,0,Order)|T2]) :- increment_clock(T,T2).

                              
								

					  
													
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):- getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):- \+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).	


runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets ; Type \= setAssoc, Num = BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).

                       
