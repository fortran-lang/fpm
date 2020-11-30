#!/usr/bin/env ruby
require 'asciidoctor'

options = {
  to_dir: File.join(__dir__, 'man1'),
  mkdirs: true,
  backend: 'manpage',
}

Dir.glob(File.join(__dir__, '*.adoc')).each do |adoc|
  Asciidoctor.convert_file adoc, options
end
