# NOTE: Apparently, the shebang line causes the nixos tests to die. It seems
# that the shebang is picking up the passed parameter (file path), which
# causes it to choke.
#
# Bizarrely, this only happens for NixOS ubuntu (CI and local). OSX works
# fine (on CI). Since we don't actually need it, it is removed.

set -e

out=$1

sleep 1

echo "Resolving dependencies..." > $out

sleep 1

echo " - bits-0.6" >> $out
echo " - byteable-0.1.1" >> $out
echo " - indexed-profunctors-0.1.1.1" >> $out
echo " - mtl-compat-0.2.2" >> $out
echo " - string-qq-0.0.6" >> $out

sleep 2

echo "Starting     bits-0.6" >> $out
echo "Starting     byteable-0.1.1" >> $out
echo "Starting     indexed-profunctors-0.1.1.1" >> $out

sleep 2

echo "Starting     mtl-compat-0.2.2" >> $out

sleep 2

echo "Completed     bits-0.6" >> $out
echo "Completed     byteable-0.1.1" >> $out

sleep 2

echo "Completed    indexed-profunctors-0.1.1.1" >> $out
echo "Starting     string-qq-0.0.6" >> $out

sleep 2

echo "Completed    mtl-compat-0.2.2" >> $out
echo "Completed    string-qq-0.0.6" >> $out

# We intend for the test to quit before this ends
sleep 10
