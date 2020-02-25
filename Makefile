build:
	mlton main.sml
	mv main 80crd

install:
	mv ./80crd /usr/local/bin
