#https://pypi.python.org/pypi/tabulate 
#http://stackoverflow.com/questions/11347505/what-are-some-approaches-to-outputting-a-python-data-structure-to-restructuredte

from __future__ import print_function
def make_table(grid):
    max_cols = [max(out)
                for out in map(list, zip(*[[len(item) for item in row]
                                           for row in grid]))]
    rst = table_div(max_cols, 1)

    for i, row in enumerate(grid):
        header_flag = False
        if i == 0 or i == len(grid) - 1: header_flag = True
        rst += normalize_row(row, max_cols)
        rst += table_div(max_cols, header_flag)
    return rst


def table_div(max_cols, header_flag=1):
    out = ""
    if header_flag == 1:
        style = "="
    else:
        style = "-"

    for max_col in max_cols:
        out += max_col * style + " "

    out += "\n"
    return out


def normalize_row(row, max_cols):
    r = ""
    for i, max_col in enumerate(max_cols):
        r += row[i] + (max_col - len(row[i]) + 1) * " "

    return r + "\n"

# def make_table(grid):
#     cell_width = 2 + max(reduce(lambda x,y: x+y, [[max(map(len, str(item).split('\n'))) for item in row] for row in grid], []))
#     num_cols = len(grid[0])
#     rst = table_div(num_cols, cell_width, 0)
#     header_flag = 1
#     for row in grid:
#         split_row = [str(cell).split('\n') for cell in row]
#         lines_remaining = 1

#         while lines_remaining>0:
#             normalized_cells = []
#             lines_remaining = 0
#             for cell in split_row:
#                 lines_remaining += len(cell)

#                 if len(cell) > 0:
#                     normalized_cell = normalize_cell(str(cell.pop(0)), cell_width - 1)
#                 else:
#                     normalized_cell = normalize_cell('', cell_width - 1)

#                 normalized_cells.append(normalized_cell)

#             rst = rst + '| ' + '| '.join(normalized_cells) + '|\n'

#         rst = rst + table_div(num_cols, cell_width, header_flag)
#         header_flag = 0
#     return rst

if __name__ == "__main__":
    print(make_table([['Name', 'Favorite Food', 'Favorite Subject'], [
        'Joe', 'Hamburgrs', 'I like things with really long names'
    ], ['Jill', 'Salads', 'American Idol'], ['Sally', 'Tofu', 'Math']]))
