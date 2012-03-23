files = FileList["src/*.hs"]
compile_cmd = "ghc -outputdir bin -isrc -Wall"

files.each do |f|
  o_file = f.pathmap("%{^src/bin}X.o")
  file o_file => f do
    sh "#{compile_cmd} #{f}"
  end
end

file "bin/Main.o" => files.dup.exclude(/Main\.hs/).pathmap("%{^src/bin}X.o")

task :compile => files.pathmap("%{^src/bin}X.o")
task :default => :compile

namespace :clean do
  task :src do
    sh "rm -rf ./bin #{files.pathmap("%X")}"
  end

  task :db do
    sh "rm -rf ./db/leveldb*"
  end

  task :all => [:src, :db]
end
task :clean => "clean:src"

task :install_deps do
  sh "cabal install json2 leveldb-haskell"
end
