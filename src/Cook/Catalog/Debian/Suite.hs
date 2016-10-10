module Cook.Catalog.Debian.Suite where

data Release = Wheezy | Jessie | Stretch
data Dist = Stable | Testing | Unstable

data Suite = Dist Dist | Release Release

wheezy, jessie, stretch, testing, stable, unstable :: Suite
wheezy = Release Wheezy
jessie = Release Jessie
stretch = Release Stretch
stable = Dist Stable
testing = Dist Testing
unstable = Dist Unstable

showSuite :: Suite -> String
showSuite (Release Wheezy) = "wheezy"
showSuite (Release Jessie) = "jessie"
showSuite (Release Stretch) = "stretch"
showSuite (Dist Stable) = "stable"
showSuite (Dist Testing) = "testing"
showSuite (Dist Unstable) = "unstable"
