source_files = FileList["*.hs"]
source_output_files = source_files.map {|source_file| [source_file, source_file.sub(/(.*)\.hs/, 'bin/\1.o')] }
output_files = source_output_files.map {|source_file, output_file| output_file }

source_output_files.each do |source_file, output_file|
  file output_file => source_file do
    sh "ghc --make -outputdir bin #{source_file}"
  end
end

task :clean do
  sh "rm -rf #{output_files.join(" ")}"
  sh "rm -rf #{source_files.map {|source_file| source_file.sub(/\.hs/, '')}.join(" ")}"
end

task :install_deps do
  sh "cabal install json2 leveldb-haskell"
end

task :compile => output_files

task :default => :compile