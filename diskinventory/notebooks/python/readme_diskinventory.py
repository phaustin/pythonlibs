# ---
# jupyter:
#   jupytext:
#     cell_metadata_filter: all
#     notebook_metadata_filter: all,-language_info
#     text_representation:
#       extension: .py
#       format_name: percent
#       format_version: '1.2'
#       jupytext_version: 1.1.5
#   kernelspec:
#     display_name: Python 3
#     language: python
#     name: python3
#   toc:
#     base_numbering: 1
#     nav_menu: {}
#     number_sections: true
#     sideBar: true
#     skip_h1_title: true
#     title_cell: Table of Contents
#     title_sidebar: Contents
#     toc_cell: true
#     toc_position:
#       height: calc(100% - 180px)
#       left: 10px
#       top: 150px
#       width: 165px
#     toc_section_display: true
#     toc_window_display: true
# ---

# %% [markdown] {"toc": true}
# <h1>Table of Contents<span class="tocSkip"></span></h1>
# <div class="toc"><ul class="toc-item"></ul></div>

# %% [markdown]
# # disk cleanup workflow

# %%
import context
import numpy as np

# %%
from diskinventory.parse_files import read_ls, read_du
print(dir(context))

# %%
du_file = context.data_dir / 'rail_users_du.txt'
du_df = read_du(du_file)
groups = du_df.groupby('level')

# %%
type(dict)

# %%
out = dict(list(groups))

# %%
level3=out[3].sort_values('size',ascending=False)
level3['size']=level3['size']*1.e-6
level3

# %%

# %% [markdown]
# ##
#
# ```
# sudo gls -R -l -Q --time-style=full-iso  /Users/ > /Users/phil/repos/pythonlibs/diskinventory/datadir/rail_users_ls.txt
# sudo gdu -k /Users/phil  > /Users/phil/repos/pythonlibs/diskinventory/datadir/rail_users_du.txt
# ```

