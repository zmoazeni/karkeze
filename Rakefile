source_files = FileList["src/*.hs"]
source_output_files = source_files.map {|source_file| [source_file, source_file.sub(/src\/(.*)\.hs/, 'bin/\1.o')] }
output_files = source_output_files.map {|source_file, output_file| output_file }

compile_cmd = "ghc -outputdir bin -isrc -Wall"

source_output_files.each do |source_file, output_file|
  file output_file => source_file do
    sh "#{compile_cmd} #{source_file}"
  end
end

task :clean do
  sh "rm -rf ./bin ./src/Main"
end

task :install_deps do
  sh "cabal install json2 leveldb-haskell"
end

task :compile => output_files

task :default => :compile
