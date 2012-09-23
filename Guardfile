# # -*- encoding : utf-8 -*-

# automatically run the eunit tests

def recompile
  cmd = "./rebar compile skip_deps=true"
  puts "Executing #{cmd}"
  puts `#{cmd}`
end

def run_eunit_all
  cmd = "./rebar eunit skip_deps=true"
  puts "Executing #{cmd}"
  puts `#{cmd}`
  if $? == 0
    n "All tests passed.", "Eunit", :success
    recompile
  else
    n "At least one test failed.", "Eunit", :failed
  end
end

def run_eunit(app_dir, src, suite)
  if File.exist?(File.join(File.dirname(__FILE__), app_dir, 'test', "#{suite}_tests.erl"))
    app = app_dir.sub(/^apps\//, '').chop
    cmd = "./rebar eunit skip_deps=true apps=#{app} suites=#{suite}"
    puts "Executing #{cmd}"
    puts `#{cmd}`
    if $? == 0
      n "#{suite}: tests passed.", "Eunit", :success
      recompile
    else
      n "#{suite}: tests failed.", "Eunit", :failed
    end
  else
    n "No tests for #{suite}.", "Eunit", :pending
    recompile
  end
end

guard 'shell' do
  watch(%r{(apps/.*?)(src|test)/([^.].*?)(_tests)?.erl}) {|m| run_eunit(m[1], m[2], m[3]) }
  watch(%r{apps/(.*?)/include/([^.].*).hrl}) {|m| run_eunit_all }
end
