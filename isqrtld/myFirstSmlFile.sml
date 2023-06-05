fun append(x,z) = if null(x) then z
else hd(x) :: append(tl(x),z); 

fun reverse(x,z) = if null(x) then z
else reverse(tl(x),hd(x)::z); 

fun rev x = reverse(x,[]);
fun append2([],z) = z
|   append2(a::y,z) = a::append2(y,z);
append2([1,2,3],[4,5]); 

fun length([]) = 0
|   length(a::y) = 1 + length(y);

length(append2([1,2,3],[4,5]));

fun strip(a,[]) = []
|   strip(a, b::y) = if a=b then strip(a,y)
                        else b::y;

strip(1,[1,1,1,3,1,4])

fun succ(x) = x + 1;

succ (succ 5);
(* 
fun mapp (strip []) = []
|   mapp(f (a::y)) = (f(a))::(mapp(f(y))) *)
map succ [1,2,3];
fun first(x,y) = x;
map first [(1,"a"), (2,"b")];

map (fn x => x*x) [3,4,5];

fun remove_if(f:('a -> bool) []) = []
|   remove_if(f (a::y)) = if (f(a)) then remove_if f(y)
                            else a::(remove_if(f(y)));

fun odd x = (x mod 2) = 1;
map odd([3,4,5,6,7,8]);

fun Merge(nil,M) = M
|   Merge(L,nil) = L
|   Merge(a::y,b::z) = if(a < b) then a::(Merge(y,b::z)) 
    else b::Merge(a::y,z);

Merge ([1,2,3,6,7],[2,4,9]);

fun split(nil) = (nil,nil)
|   split([a]) = ([a],nil)
|   split(a::b::rest) =
    let 
    val (M,N) = split(rest);
    in 
    (a::M,b::N)
    end;

fun MergeSort(nil) = nil
|   MergeSort([a]) = [a]
|   MergeSort(L) = 
    let 
        val (M,N) = split(L);
    in 
        Merge(MergeSort(M),MergeSort(N))
    end; 

MergeSort [4,13,5,1,30,29,12,39,34,12];

(* to do a sequence of executions use the (expression1; expression2; ... lastexpression; ); 
syntax. the value of the whole will be the last one's value *)

fun printList([]) = ()
|   printList(x::y) = (
    print(Int.toString(x));
    print("\n");
    printList(y)
    );

printList [1,2,3,4]; 

fun exponent2 x 0 = 1
|   exponent2 x y = ((x * exponent2 x (y-1)) mod MOD);

val MOD : int = 10007;
fun binExp2 x 0 = 1
|   binExp2 x y = if ((y mod 2) = 0) then 
    let 
        val k : int = binExp2 x (y div 2);
        val kk : int = k*k;
    in
        (kk mod MOD)
    end
    else ((x * binExp2 x (y-1)) mod MOD);
binExp2 7 20000000;

fun squared x = binExp2 x 2;
fun cube x = binExp2 x 3;
print (" gap \n");

fun comp(F,G,x) = F(G(x)); (* function composition *)

(* the o operator works the same, but is better because it returns one function*)
val pow6 = cube o squared;
fun actualComp F G =
    let
        fun C x = F(G(x));
    in
        C
    end;
val pow6 = actualComp cube squared;
val k:char list = rev o (map ( hd o explode)) ["Joi", "bio"]; (*returns a list of characters*)
rev k;
rev [1,2,3,4];
print();

