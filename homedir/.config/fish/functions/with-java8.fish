# Defined in /tmp/fish.iIFvBL/with-java8.fish @ line 2
function with-java8
	begin
        set -lx PATH /usr/lib/jvm/java-8-openjdk/bin $PATH
        eval $argv
    end
end
