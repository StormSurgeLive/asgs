build-1111:
	docker build --build-arg _ASGS_BRANCH=issue-1111 -t asgs:debian -f ./Dockerfile.debian .

build:
	docker build --build-arg _ASGS_BRANCH=master -t asgs:debian -f ./Dockerfile.debian .

clean-build-1111:
	docker build --no-cache --build-arg _ASGS_BRANCH=issue-1111 -t asgs:debian -f ./Dockerfile.debian .

clean-build:
	docker build --no-cache -t asgs:debian -f ./Dockerfile.debian .

start:
	docker-compose up -d asgs-debian

verify:
	docker exec -it asgs-debian bin/asgsh -v

showkey:
	docker exec -it asgs-debian cat /home/asgsuser/.ssh/id_rsa.pub

copykey:
	docker cp ~/.ssh/id_rsa asgs-debian:/home/asgsuser/.ssh/id_rsa
	docker cp ~/.ssh/id_rsa.pub asgs-debian:/home/asgsuser/.ssh/id_rsa.pub
	docker exec -it asgs-debian chmod 600 /home/asgsuser/.ssh/id_rsa
	docker exec -it asgs-debian chmod 600 /home/asgsuser/.ssh/id_rsa.pub

shell:
	docker exec -it asgs-debian bash 

asgsh:
	docker exec -it asgs-debian asgs/asgsh

stop:
	docker-compose down
