import shelve

def store_variable(shelve_dir, current_variable_names, variables_dict, option = 'n'):
    filename = shelve_dir
    my_shelf = shelve.open(filename,option) # 'n' for new

    for key in current_variable_names:
        try:
            my_shelf[key] = variables_dict[key]
        except TypeError:
            #
            # __builtins__, my_shelf, and imported modules can not be shelved.
            #
            print('ERROR shelving: {0}'.format(key))
    my_shelf.close()
    print('Stored Successfully')

def retrieve_variable(shelve_dir, key):
    filename = shelve_dir
    my_shelf = shelve.open(filename)
    

    if key in my_shelf.keys():
        tmp = my_shelf[key]
        my_shelf.close()
        print('Retrieved Successfully')
        return tmp
    print('Retrieved Failed')
    return NULL
