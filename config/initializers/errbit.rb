Airbrake.configure do |config|
  config.api_key = '849c05f51633ebfe92d51dc31739e6de'
  config.host    = 'errbit.ps.informatik.uni-kiel.de'
  config.port    = 80
  config.secure  = config.port == 443
end
