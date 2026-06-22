# mvn_install is a script to build and run using maven
# Will prompt an error if failed to build the app
# Receive the folder where the app is supposed to be runned ($1)
mvn clean install

if [ $? -eq 0 ]; then
  cd app
  mvn spring-boot:run
  cd ..

else
  echo "Failed to build app"
fi
