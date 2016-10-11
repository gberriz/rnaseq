import sys
import os
import collections
import pandas

import misc

# python file_organization/src/tabulate.py \
#        file_organization/input/main/views/2 \
#        $HOME/public_html/gbm_immunosuppression/find_zero/qc

# -----------------------------------------------------------------------------

HALFINDENT = '  '

class Row(object):
    def __init__(self, depth,
                 colnames=[None], cells={}, label='', is_header=False):

        assert not ((cells or label) and is_header)

        assert set(cells.keys()).issubset(set(colnames))

        self.depth = depth
        self.colnames = colnames
        self.cells = cells
        self.label = '' if not label else label
        self.is_header = is_header

        self.indent = HALFINDENT * 2 * depth
        self.cell_tag = 'th' if is_header else 'td'

    def __str__(self):
        indent = self.indent
        cell_tag = self.cell_tag
        cells = self.cells

        ret = []

        ret.append('%s<tr class="depth-%d">\n' % (indent, self.depth))
        label_cell = ('%s%s<%s>%s</%s>\n' %
                      (indent, HALFINDENT, cell_tag, self.label, cell_tag))

        ret.append(label_cell)


        if self.is_header:
            for colname in self.colnames:
                ret.append('%s%s<%s>%s</%s>\n' %
                           (indent, HALFINDENT, cell_tag, colname, cell_tag))
        else:

            for colname in self.colnames:
                ret.append('%s%s<%s>\n' % (indent, HALFINDENT, cell_tag))
                ret.append(str(cells.get(colname, '')))
                ret.append('%s%s</%s>\n' % (indent, HALFINDENT, cell_tag))


        ret.append(label_cell)

        ret.append('%s</tr>\n' % indent)

        return ''.join(ret)


class Table(object):
    def __init__(self, depth, rows):
        self.rows = rows
        self.depth = depth
        self.indent = HALFINDENT * 2 * depth

    def __str__(self):
        indent = self.indent
        ret = []

        ret.append('%s<table class="depth-%d">\n' % (indent, self.depth))
        ret.append('%s%s<tbody>\n' % (indent, HALFINDENT))

        for item in self.rows:
            ret.append(str(item))

        ret.append('%s%s</tbody>\n' % (indent, HALFINDENT))
        ret.append('%s</table>\n' % indent)

        return ''.join(ret)


class Img(object):
    def __init__(self, depth, keys, value):
        self.title = ' '.join(keys)
        self.value = value
        self.indent = HALFINDENT * 2 * depth

    def __str__(self):
        ret = []
        ret.append('%s<img class="thumbnail" src="%s" title="%s"/>\n' %
                   (self.indent, self.value, self.title))
        return ''.join(ret)

# -----------------------------------------------------------------------------

def make_dataframe(rows, column_names):
    return pandas.DataFrame(
               collections.OrderedDict([(column_name,
                                         [row[i] for row in rows])
                                       for i, column_name
                                       in enumerate(column_names)])
           )

def is_png_file(_, basename):
    return basename.endswith('.png')

def is_thumbnail(_, basename):
    return basename.startswith('thumbnail')

def get_files(inputbase, is_wanted_file):
    for path_, _, filenames in os.walk(inputbase, followlinks=True):
        relative_path = os.path.relpath(path_, start=inputbase)
        for filename in filenames:
            if is_wanted_file(relative_path, filename):
                if relative_path == os.curdir:
                    yield filename
                else:
                    yield os.path.join(relative_path, filename)

# -----------------------------------------------------------------------------

def get_groups(dataframe, by):
    groups = dataframe.groupby(by).groups
    return collections.OrderedDict([(key, dataframe.loc[groups[key], :])
                                    for key in sorted(groups.keys())])


def tabulate(dataframe, columns, callback, orientation='V'):

    def _tabulate_odd(dataframe, columns, depth, max_depth):

        max_depth[0] = max(depth, max_depth[0])

        groups = get_groups(dataframe, columns[0])

        cells = (collections.
                 OrderedDict([(key, _tabulate_even(subdf,
                                                   columns[1:],
                                                   depth=depth + 2,
                                                   max_depth=max_depth))
                              for key, subdf in groups.items()]))

        if orientation == 'V':

            rows = [Row(depth + 1, cells={None: content}, label=key)
                    for key, content in cells.items()]


        else:

            assert orientation == 'H'
            colnames = cells.keys()
            header = Row(depth + 1, colnames=colnames, is_header=True)
            rows = [header,
                    Row(depth + 1, colnames=colnames, cells=cells),
                    header]

        return Table(depth, rows=rows)


    def _tabulate_even(dataframe, columns, depth, max_depth):

        max_depth[0] = max(depth, max_depth[0])

        if len(columns) == 0:
            assert dataframe.shape[0] == 1
            return callback(dataframe, depth)

        groups = get_groups(dataframe, columns[0])

        colname_set = set()
        for subdf in groups.values():
            colname_set.update(get_groups(subdf, columns[1]).keys())

        colnames = sorted(colname_set)
        header_row = Row(depth + 1, colnames=colnames, is_header=True)

        rows = []
        rows.append(header_row)

        for key, subdf in groups.items():

            cells = {colname: _tabulate_even(subdf1,
                                             columns[2:],
                                             depth=depth + 2,
                                             max_depth=max_depth)
                     for colname, subdf1
                         in get_groups(subdf, columns[1]).items()}

            rows.append(Row(depth + 1,
                            colnames=colnames,
                            cells=cells,
                            label=key))

        rows.append(header_row)

        return Table(depth, rows=rows)

    # -------------------------------------------------------------------------



    max_depth = [-1]

    if len(columns) % 2 == 1:
        table = _tabulate_odd(dataframe, columns, 0, max_depth=max_depth)
    else:
        table = _tabulate_even(dataframe, columns, 0, max_depth=max_depth)

    return {'table': table, 'max_depth': max_depth[0]}


# -----------------------------------------------------------------------------

def dump_style(font_family, max_depth, output):

    caption_size = 8
    base_font_size = 10
    # font_size_increment = base_font_size - caption_size

    base_border_width = 1
    base_padding = 15

    print >> output, \
'''
* {
  font-family: %(font_family)s, monospace;
}

td:first-child, td:last-child {
  font-weight: bold;
}

table {
  border-collapse: collapse;
}

.caption {
  color: #999;
}

td {
  border-color: #555;
  border-style: solid;
  text-align: center;
}

th, td:first-child, td:last-child {
  border-width: 0px;
  border-style: hidden;
}

.caption {
  font-size: %(caption_size)dpx;
}

''' % locals()

    # font_size = base_font_size + font_size_increment
    font_size = base_font_size
    border_width = base_border_width
    padding = base_padding

    styles = []

    for depth in range(max_depth - 1, 0, -2):

        print >> output, '''

tr.depth-%(depth)d > td, tr.depth-%(depth)d > th {
  font-size: %(font_size)dpx;
}

tr.depth-%(depth)d > td:first-child {
  padding-left: %(padding)dpx;
}

tr.depth-%(depth)d > td:last-child {
  padding-right: %(padding)dpx;
}

tr.depth-%(depth)d:first-child > th {
  padding-top: %(padding)dpx;
}

tr.depth-%(depth)d:last-child > th {
  padding-bottom: %(padding)dpx;
}

tr.depth-%(depth)d > td:not(first-child):not(last-child) {
  border-width: %(border_width)dpx;
}

''' % locals()

        # font_size = font_size + font_size_increment
        font_size = 2 * font_size
        border_width = 2 * border_width
        padding = 2 * padding


def dump_epilogue(output):

    print >> output, '''
<div id="popup" class="overlay">
  <div class="wrapper">
    <div>
      <div class="title-div"><span class="title"></span></div>
      <img/>
    </div>
  </div>
</div>

<script src="//code.jquery.com/jquery-3.1.0.min.js"
        integrity="sha256-cCueBR6CsyA4/9szpPfrX3s49M9vUU5BgtiJj06wt/s="
        crossorigin="anonymous"></script>

<script src="../../js/code.js"></script>
'''

def dump_table(dataframe, key_columns, callback, outputdir, **kwargs):

    misc.mkdirp(outputdir)
    output_filename = os.path.join(outputdir, 'index.html' )

    tabulation = tabulate(dataframe, key_columns, callback, **kwargs)
    table = tabulation['table']
    max_depth = tabulation['max_depth']
    # font_family = 'Inconsolata'
    font_family = 'Courier'

    with open(output_filename, 'w') as output:

        # print >> output, ('''<link href="https://fonts.googleapis.com/css'''
        #                   '''?family=%s" rel="stylesheet">''' % font_family)

        print >> output, '''<link rel="stylesheet" href="../../css/style.css"/>'''

        print >> output, '<style>\n'
        dump_style(font_family, max_depth, output)
        print >> output, '</style>\n'

        print >> output, table

        dump_epilogue(output)

# -----------------------------------------------------------------------------

def main():
    inputbase = sys.argv[1]
    outputdir = sys.argv[2]

    exploded_paths = [path_.split('/')[:-1] +
                      [os.path.join('..', '..', 'img', path_)]
                      for path_ in get_files(inputbase, is_thumbnail)]

    column_names = 'number_of_genes expected_number_of_counts_per_gene method treatment'.split()
    dataframe = make_dataframe(exploded_paths, column_names + ['value'])

    key_columns = 'number_of_genes expected_number_of_counts_per_gene treatment method'.split()
    def callback(dataframe, depth):
        keys = dataframe.loc[:, key_columns].values[0]
        value = dataframe.loc[:, 'value'].values[0]
        return Img(depth, keys, value)

    dump_table(dataframe, key_columns, callback,
               os.path.join(outputdir, 'plots', '__ALL__'),
               orientation='H')

    # first_key_column = key_columns[0]
    # trailing_key_columns = key_columns[1:]

    # for channel, group in dataframe.groupby(first_key_column).groups.items():

    #     subdataframe = dataframe.loc[group, :]
    #     plotdir = os.path.join(outputdir, 'plots', channel)

    #     dump_table(subdataframe,
    #                ['tissue', 'status', 'day', 'replicate'],
    #                callback,
    #                plotdir)

main()
