all:
	# базовый слой для всех сервисов, с библиотеками-зависимостями
	docker build -t svetlyak40wt/tender-2022-base:latest -f docker/Dockerfile.base .
	# Сервисы
	docker build -t svetlyak40wt/tender-2022-app:latest -f docker/Dockerfile --build-arg APP=app .
