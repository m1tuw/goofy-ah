import scala.collection.mutable.ArrayBuffer

class SegmentTree(var vec: ArrayBuffer[Int], val merge: (Int, Int) => Int) {
	private var n: Int = vec.size;
	private var tree: ArrayBuffer[Int] = ArrayBuffer.fill(n << 1)(0);
	init();
	
	def init(): Unit = {
		for(i <- n until 2*n){
			tree(i) = vec(i-n);
		}
		for(i <- (n-1 until 0 by -1)){
			val left_child: Int = 2*i;
			val right_child: Int = 2*i + 1;
			tree(i) = merge(tree(left_child), tree(right_child));
		}
	}
	
	def query(L: Int, R: Int): Int = {
		var ans: Int = 0; // identity element
		var l: Int = L + n;
		var r: Int = R + n;
		while(l <= r) {
			if (l % 2 == 1) {
				ans = merge(ans, tree(l));
				l += 1;
			}
			if (r % 2 == 0) {
				ans = merge(ans, tree(r));
				r -= 1;
			}
			l >>= 1;
			r >>= 1;
		}
		return ans;
	}
	
	def update(pos: Int, x: Int): Unit = {
		var i: Int = pos + n;
		tree(i) = x;
		while(i > 1){
			i >>= 1;
			val left_child = 2*i;
			val right_child = 2*i + 1;
			tree(i) = merge(tree(left_child), tree(right_child));
		}
	}
	
	def print(): Unit = {
		println(tree);
	}
}
