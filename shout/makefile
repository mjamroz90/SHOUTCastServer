
compile:
	erlc -o ./ebin ./src/libs/*.erl ./src/logging/*.erl ./src/servers/*.erl ./src/supervisors/*.erl
	erlc -o ./ebin ./src/*.erl

run: 
	werl -pa ./ebin -boot start_sasl -config ./log/elog

clean: 
	rm ./ebin/*.beam



