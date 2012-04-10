#include "erl_nif.h"
#include <lua.h>
#include <lualib.h>
#include <lauxlib.h>
#include <string.h>

static ErlNifResourceType* nodelua_RESOURCE = NULL;

typedef struct
{
    lua_State *lua;
} nodelua_handle;

// Prototypes
static ERL_NIF_TERM nodelua_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM nodelua_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, nodelua_new},
    {"myfunction", 1, nodelua_myfunction}
};

static ERL_NIF_TERM nodelua_new(ErlNifEnv* env, int argc,
                                   const ERL_NIF_TERM argv[])
{
    nodelua_handle* handle = enif_alloc_resource(nodelua_RESOURCE,
                                                    sizeof(nodelua_handle));

    handle->lua = luaL_newstate();
    luaL_openlibs(handle->lua);

    int error = 0;
    const char buff[256] = "print('this is a test')";
    
    error = luaL_loadbuffer(handle->lua, buff, strlen(buff), "line") || lua_pcall(handle->lua, 0, 0, 0);
    if (error) {
        fprintf(stderr, "%s", lua_tostring(handle->lua, -1));
        lua_pop(handle->lua, 1); /* pop error message from the stack */
    }

    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource(handle);
    return enif_make_tuple2(env, enif_make_atom(env, "ok"), result);
}


static ERL_NIF_TERM nodelua_myfunction(ErlNifEnv* env, int argc,
                                          const ERL_NIF_TERM argv[])
{
    return enif_make_atom(env, "ok");
}

static void nodelua_resource_cleanup(ErlNifEnv* env, void* arg)
{
    /* Delete any dynamically allocated memory stored in nodelua_handle */
    nodelua_handle* handle = (nodelua_handle*)arg;
    lua_close(handle->lua);
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
                                                     "nodelua_resource",
                                                     &nodelua_resource_cleanup,
                                                     flags, NULL);
    if (rt == NULL)
        return -1;

    nodelua_RESOURCE = rt;

    return 0;
}

ERL_NIF_INIT(nodelua, nif_funcs, &on_load, NULL, NULL, NULL);
