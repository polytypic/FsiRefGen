require 'bundler/setup'

require 'albacore'
require 'albacore/tasks/release'
require 'albacore/tasks/versionizer'
require 'albacore/ext/teamcity'

Configuration = ENV['CONFIGURATION'] || 'Release'

Albacore::Tasks::Versionizer.new :versioning

desc 'create assembly infos'
asmver_files :assembly_info do |a|
  a.files = FileList['FsLibTool/*.fsproj']

  a.attributes assembly_description: 'A documentation generator for .fsi files for F#',
               assembly_configuration: Configuration,
               assembly_copyright: "(c) 2015 by Vesa Kaarvonen",
               assembly_version: ENV['LONG_VERSION'],
               assembly_file_version: ENV['LONG_VERSION'],
               assembly_informational_version: ENV['BUILD_VERSION']
  a.handle_config do |proj, conf|
    conf.namespace = proj.namespace + "Asm"
    conf
  end
end

desc 'Perform fast build (warn: doesn\'t d/l deps)'
build :quick_compile do |b|
  b.prop 'Configuration', Configuration
  b.logging = 'detailed'
  b.sln     = 'FsLibTool/FsLibTool.sln'
end

task :paket_bootstrap do
  system 'tools/paket.bootstrapper.exe', clr_command: true \
    unless File.exists? 'tools/paket.exe'
end

desc 'restore all nugets as per the packages.config files'
task :restore => :paket_bootstrap do
  system 'tools/paket.exe', 'restore', clr_command: true
end

desc 'Perform full build'
build :compile => [:versioning, :restore, :assembly_info] do |b|
  b.prop 'Configuration', Configuration
  b.sln = 'FsLibTool/FsLibTool.sln'
end

directory 'build/pkg'

desc 'package nugets - finds all projects and package them'
nugets_pack :create_nugets => ['build/pkg', :versioning, :compile] do |p|
  p.configuration = Configuration
  p.files   = FileList['FsLibTool/*.fsproj'].exclude(/Tests/)
  p.out     = 'build/pkg'
  p.exe     = 'packages/NuGet.CommandLine/tools/NuGet.exe'
  p.with_metadata do |m|
    m.id          = 'FsLibTool'
    m.title       = 'FsLibTool'
    m.description = 'A tool that simply generates a nice doc file for a project\'s .fsi files'
    m.authors     = 'Vesa Kaarvonen'
    m.project_url = 'https://github.com/polytypic/FsLibTool'
    m.tags        = 'fsharp docs'
    m.version     = ENV['NUGET_VERSION']
  end
  p.with_package do |pkg|
    pkg.add_file "bin/#{Configuration}/FsLibTool.exe", 'tools'
    pkg.add_file "bin/#{Configuration}/FsLibTool.exe.config", 'tools'
    pkg.add_file "bin/#{Configuration}/FsLibTool.exe.mdb", 'tools'
    pkg.add_file "bin/#{Configuration}/PPrint.dll", 'tools'
    pkg.add_file "bin/#{Configuration}/FSharp.Core.dll", 'tools'
  end
end

namespace :tests do
  #task :unit do
  #  system "src/MyProj.Tests/bin/#{Configuration}/MyProj.Tests.exe", clr_command: true
  #end
end

# task :tests => :'tests:unit'

task :default => :create_nugets #, :tests ]

task :ensure_nuget_key do
  raise 'missing env NUGET_KEY value' unless ENV['NUGET_KEY']
end

Albacore::Tasks::Release.new :release,
                             pkg_dir: 'build/pkg',
                             depend_on: [:create_nugets, :ensure_nuget_key],
                             nuget_exe: 'packages/NuGet.CommandLine/tools/NuGet.exe',
                             api_key: ENV['NUGET_KEY']
