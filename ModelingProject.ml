(* 
	Christian Cameron Modeling Project. 
	Modeling Pro Athletes
	____________________

	Modeling 7 types of professional athletes
	Each type has basic generic traits that makes them
	 preform better.
*)

type name = string;

type liftMax = int;

(*A type holding the accuracy percentage of an athlete
  accuracy must be between 0-1*)
type accuracy = real;

type verticalJump = real;

(*Time of releasing a certain object. Used for quarterbacks*)
type releaseTime = real;

(*Speed of the athlete, read as MPH- swimmers are given a multiplier*)
type speed = real;

type weight = real;

type failPercentage = real;

(*
	Modeling the type of athlete. If a sport has multiple positions it
	will be split up.

	-Wide receivers and running backs have the same traits
*)
datatype athlete = 
	  WideReceiver of name * liftMax * verticalJump * speed
	| Quarterback of name * accuracy * releaseTime * failPercentage
	| OffensiveCenter of name * liftMax * weight
	| BasketballPlayer of name * accuracy * verticalJump * speed
	| Swimmer of name * verticalJump * speed
	| Runner of name * speed
	| Powerlifter of name * liftMax;

	
(*Modeling Data*)

val leonardFournette = WideReceiver("Leonard Fournette", 1665, 33.0, 17.54);
val randyMoss = WideReceiver("Randy Moss", 1000, 51.0, 18.82);

val aaronRodgers = Quarterback("Aaron Rodgers", 0.663, 2.2, 0.015);
val tomBrady = Quarterback("Tom Brady", 0.638, 2.5, 0.017);

val dermonttiDawson = OffensiveCenter("Dermontti Dawson", 1750, 320.0);
val mikeWebster = OffensiveCenter("Mike Webster", 2120, 312.0);
val jimOtto = OffensiveCenter("Jim Otto", 1970, 256.0);

val lebronJames = BasketballPlayer("LeBron James", 0.784, 40.0, 20.0);
val kobeBryant = BasketballPlayer("Kobe Bryant", 0.826, 38.0, 15.5);
val michaelJordan = BasketballPlayer("Michael Jordan", 0.821, 48.0, 19.13);

val caelebDressel = Swimmer("Caeleb Dressel", 41.0, 4.742);
val michaelPhelps = Swimmer("Michael Phelps", 37.0, 4.709);

val usainBolt = Runner("Usain Bolt", 23.35);
val justinGatlin = Runner("Justin Gatlin", 22.71);

val eddieHall = Powerlifter("Eddie Hall", 2653);
val brianShaw = Powerlifter("Brian Shaw", 2685);



val allAthletes = [aaronRodgers, tomBrady, lebronJames, kobeBryant, michaelJordan, leonardFournette, caelebDressel, michaelPhelps, usainBolt,
				eddieHall, brianShaw, randyMoss, justinGatlin, dermonttiDawson, mikeWebster, jimOtto];

val allFBAthletes = [aaronRodgers, tomBrady, leonardFournette, randyMoss, dermonttiDawson, mikeWebster, jimOtto];


(*
Functions applying the data
	n = name
	a = accuracy
	m = liftmax
	vj = jump
	s = speed
	r = reactionspeed
	rt = releasetime
	fp = failpercentage
	w = weight
*)

(*Calculate the power index of the athlete. Returns a int.*)
fun calculatePowerIndex(WideReceiver(n,m,vj,s)) = Real.fromInt(m div 50)+(vj*5.6)+(s*9.0)
	| calculatePowerIndex(Quarterback(n,a,rt,fp)) = a*870.0 - rt*50.0 - fp*1000.0
	| calculatePowerIndex(OffensiveCenter(n,m,w)) = (Real.fromInt(m)*0.5*w*0.05)/36.0
	| calculatePowerIndex(BasketballPlayer(n,a,vj,s)) = (vj*3.0 + s)*(a*3.9)
	| calculatePowerIndex(Swimmer(n,vj,s)) = ((s*84.0) + (vj*2.0))
	| calculatePowerIndex(Runner(n,s)) = s*20.7
	| calculatePowerIndex(Powerlifter(n,m)) = Real.fromInt(m)/5.9;

(*
Sort athletes by power index.
	-Takes a list of Athletes
	-Returns a list of Athletes formated by (string concatenated with power index)
Sorting method used is insertion sort.
*)
fun sort(athletes) = 
	let fun names([]) = []
	| names(WideReceiver(n,m,vj,s)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(WideReceiver(n,m,vj,s)))))::names(rest)
	| names(Quarterback(n,a,rt,fp)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(Quarterback(n,a,rt,fp)))))::names(rest)
	| names(OffensiveCenter(n,m,w)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(OffensiveCenter(n,m,w)))))::names(rest)
	| names(BasketballPlayer(n,a,vj,s)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(BasketballPlayer(n,a,vj,s)))))::names(rest)
	| names(Swimmer(n,vj,s)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(Swimmer(n,vj,s)))))::names(rest)
	| names(Runner(n,s)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(Runner(n,s)))))::names(rest)
	| names(Powerlifter(n,m)::rest) = (n^": "^Int.toString(round(calculatePowerIndex(Powerlifter(n,m)))))::names(rest)

	fun insertionSort([]) = []
	| insertionSort(x::rest) = 
		let fun insert(x, []) = [x]
			| insert(x, y::rest) = 
			if (calculatePowerIndex(x) > calculatePowerIndex(y))
				then x::y::rest
			else y::insert(x,rest)
		in
			insert(x,insertionSort(rest))
	end
	in
		names(insertionSort(athletes))
end;

