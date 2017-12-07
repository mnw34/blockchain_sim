
test : account_test subdir_tests

account_test : account_test.scala account.scala transfer.scala dumbSignatures.scala crypto/*.scala 
	@scalac $^
	@scala $@ 10 
	
subdir_tests : trees_tests

trees_tests : 
	$(MAKE) -C trees test 

