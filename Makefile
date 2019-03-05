PROJECT = tcp_acceptor

# --------------------------------------------------------------------                                                  
#  # Defining OTP version for this project which uses by kerl                                                              
# --------------------------------------------------------------------                                                  
ERLANG_OTP = OTP-21.0                                                                                                   

#ERLC_OPTS += +warn_export_all +warn_export_vars +warn_unused_import +warn_untyped_record +warn_missing_spec_all         

dep_lager = git https://github.com/erlang-lager/lager
dep_teaser = git https://github.com/spylik/teaser
dep_sync = git https://github.com/rustyio/sync

DEPS = lager teaser
SHELL_DEPS =  sync

SHELL_OPTS = -pa ebin/ test/ -env ERL_LIBS deps -eval 'lager:start(), mlibs:discover()' -env ERL_LIBS deps -run mlibs autotest_on_compile

include erlang.mk
