
test : merkelTree_test rbt_test bst_test 

merkelTree_test : merkelTree_test.scala merkelTree.scala ../crypto/hash.scala bst.scala
	@scalac $^
	@scala $@ 10 10 | grep "PASSED" > /dev/null && echo "$@ PASSED"

rbt_test : rbt_test.scala rbt.scala bst.scala
	@scalac $^
	@scala $@ 10 10 | grep "PASSED" > /dev/null && echo "$@ PASSED"

bst_test : bst_test.scala bst.scala
	@scalac $^
	@scala $@ 10 10 | grep "PASSED" > /dev/null && echo "$@ PASSED"




