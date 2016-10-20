#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
    def __init__(self,
                 depth,
                 is_singleton,
                 colnames=[None],
                 cells={},
                 label='',
                 tooltip=None,
                 is_header=False):

        # h = is_header
        # s = is_singleton
        # t = bool(tooltip)
        # f = (h or not s)
        # g = (t == f)

        # h s t   f g
        # F F F ✗ T F
        # F F T ✓ T T
        # F T F ✓ F T
        # F T T ✗ F F
        # T F F ✗ T F
        # T F T ✓ T T
        # T T F ✗ T F
        # T T T ✓ T T

        assert bool(tooltip) == (is_header or not is_singleton)

        assert not ((cells or label) and is_header)
        assert (is_singleton or is_header) == (not label)

        assert set(cells.keys()).issubset(set(colnames))

        self.depth = depth
        self.is_singleton = is_singleton
        self.colnames = colnames
        self.cells = cells
        self.label = '' if not label else label
        self.tooltip = tooltip
        self.is_header = is_header

        self.indent = HALFINDENT * 2 * depth
        self.cell_tag = 'th' if is_header else 'td'


    def __str__(self):
        indent = self.indent
        cell_tag = self.cell_tag
        cells = self.cells

        ret = []

        ret.append('%s<tr class="depth-%d">\n' % (indent, self.depth))

        if self.is_singleton:
            label_cell = []
        elif self.is_header:
            label_cell = [('%s%s<%s></%s>\n' %
                           (indent, HALFINDENT, cell_tag, cell_tag))]
        else:
            # label_cell = [('%s%s<%s><span class="header" title="%s">%s</span></%s>\n' %
            #                (indent, HALFINDENT, cell_tag, self.tooltip,
            #                 self.label, cell_tag))]

            label_cell = [('%s%s<%s class="header" title="%s">%s</%s>\n' %
                           (indent, HALFINDENT, cell_tag, self.tooltip,
                            self.label, cell_tag))]


        ret.extend(label_cell)

        if self.is_header:

            for colname in self.colnames:
                # ret.append('%s%s<%s><span class="header" title="%s">%s</span></%s>\n' %
                #            (indent, HALFINDENT, cell_tag,
                #             self.tooltip, colname, cell_tag))

                ret.append('%s%s<%s class="header" title="%s">%s</%s>\n' %
                           (indent, HALFINDENT, cell_tag,
                            self.tooltip, colname, cell_tag))
        else:

            for colname in self.colnames:
                ret.append('%s%s<%s>\n' % (indent, HALFINDENT, cell_tag))
                ret.append(str(cells.get(colname, '')))
                ret.append('%s%s</%s>\n' % (indent, HALFINDENT, cell_tag))


        ret.extend(label_cell)

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

        number_of_cells = len(cells.keys())
        assert number_of_cells > 0

        if orientation == 'V':

            is_singleton = number_of_cells == 1
            tooltip = '' if is_singleton else columns[0]

            rows = [Row(depth + 1,
                        is_singleton=is_singleton,
                        cells={None: content},
                        tooltip=tooltip,
                        label=key)
                    for key, content in cells.items()]

        else:

            assert orientation == 'H'
            colnames = cells.keys()
            header = Row(depth + 1, is_singleton=True,
                         colnames=colnames,
                         tooltip=columns[0],
                         is_header=True)
            rows = [header,
                    Row(depth + 1, is_singleton=True,
                        colnames=colnames, cells=cells),
                    header]

        return Table(depth, rows=rows)


    def _tabulate_even(dataframe, columns, depth, max_depth):

        max_depth[0] = max(depth, max_depth[0])

        if len(columns) == 0:
            assert dataframe.shape[0] == 1
            return callback(dataframe, depth)

        groups = get_groups(dataframe, columns[0])

        number_of_rows = len(groups.keys())
        assert number_of_rows > 0

        is_singleton = number_of_rows == 1

        colname_set = set()
        for subdf in groups.values():
            colname_set.update(get_groups(subdf, columns[1]).keys())

        colnames = sorted(colname_set)


        if len(colnames) < 2:
            header_row = []
        else:
            header_row = [Row(depth + 1,
                              is_singleton,
                              colnames=colnames,
                              tooltip=columns[1],
                              is_header=True)]

        rows = []
        rows.extend(header_row)

        for key, subdf in groups.items():

            cells = {colname: _tabulate_even(subdf1,
                                             columns[2:],
                                             depth=depth + 2,
                                             max_depth=max_depth)
                     for colname, subdf1
                         in get_groups(subdf, columns[1]).items()}

            rows.append(Row(depth + 1,
                            is_singleton,
                            colnames=colnames,
                            cells=cells,
                            label=key,
                            tooltip=columns[0]))

        rows.extend(header_row)

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
    # base_margin = 15
    # base_padding = 15
    base_spacing = 15

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

    # padding = base_padding
    # margin = base_margin
    spacing = base_spacing

    styles = []

    for depth in range(max_depth - 1, 0, -2):

        print >> output, '''

tr.depth-%(depth)d > td, tr.depth-%(depth)d > th {
  font-size: %(font_size)dpx;
}

tr.depth-%(depth)d > td:first-child > .header {
  margin-left: %(spacing)dpx;
}

tr.depth-%(depth)d > td:last-child > .header {
  margin-right: %(spacing)dpx;
}

tr.depth-%(depth)d:first-child > th > .header {
  margin-top: %(spacing)dpx;
}

tr.depth-%(depth)d:last-child > th > .header {
  margin-bottom: %(spacing)dpx;
}

tr.depth-%(depth)d > td:not(first-child):not(last-child) {
  border-width: %(border_width)dpx;
}

''' % locals()

        # font_size = font_size + font_size_increment
        font_size = 2 * font_size
        border_width = 2 * border_width
        # padding = 2 * padding
        # margin = 2 * margin
        spacing = 2 * spacing

def dump_table(dataframe, key_columns, callback, basedir, outputdir, **kwargs):

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

        css = os.path.relpath(os.path.join(basedir, 'css', 'style.css'),
                              outputdir)

        print >> output, '<link rel="stylesheet" href="%s"/>' % css

        print >> output, '<style>\n'
        dump_style(font_family, max_depth, output)
        print >> output, '</style>\n'

        print >> output, table

        print >> output, '''
<div id="popup">
  <div class="title-div"><span class="title"></span></div><br/>
  <img/>
</div>

<script src="//code.jquery.com/jquery-3.1.0.min.js"
        integrity="sha256-cCueBR6CsyA4/9szpPfrX3s49M9vUU5BgtiJj06wt/s="
        crossorigin="anonymous"></script>
'''
        js = os.path.relpath(os.path.join(basedir, 'js', 'code.js'),
                             outputdir)
        print >> output, '<script src="%s"></script>' % js

# -----------------------------------------------------------------------------

def path_to_tuple(path):
    return tuple(path.split(os.path.sep))

def annotate_paths(paths):

    def parse_metadata(path):

        def _parse_metadata(path):

            if path == '' or path == '.':
                return []

            if os.path.isdir(path):
                factordir = os.path.join(path, '.factor')
                if os.path.isdir(factordir):
                    level = os.path.basename(path)
                    factor = os.listdir(factordir)[0]
                    pair = [(level, factor)]
                else:
                    pair = []
            else:
                pair = [(path, 'value')]

            return _parse_metadata(os.path.dirname(path)) + pair

        return zip(*_parse_metadata(path))

    number_of_paths = len(paths)

    rows, column_names = zip(*map(parse_metadata,
                                  sorted(paths, key=path_to_tuple)))

    keys = [tuple(row[:-1]) for row in rows]
    number_of_keys = len(set(keys))

    if ((number_of_paths != number_of_keys) or
        len(set(column_names)) != 1):

        raise ValueError('malformed input')

    return make_dataframe(rows, column_names[0])


def main():
    import fileinput

    # if hasattr(sys, 'original_argv'):
    #     sys.argv = sys.original_argv[:]
    # else:
    #     sys.original_argv = sys.argv[:]

    basedir = sys.argv.pop(1)
    outputdir = sys.argv.pop(1)

    def convert(line):
        path = line.rstrip('\r\n')
        return path
        # path = os.path.normpath(os.path.join(basedir, line.rstrip('\r\n')))
        # return os.path.relpath(path, outputdir)

    paths = map(convert, fileinput.input())
    os.chdir(basedir)
    annotated = annotate_paths(paths)

    # annotated.to_csv(sys.stdout, sep='\t', index=False)
    # return

    key_columns = list(annotated.columns)[:-1]

    def callback(dataframe, depth):
        keys = dataframe.loc[:, key_columns].values[0]
        subpath = dataframe.loc[:, 'value'].values[0]
        fullpath = os.path.join(basedir, subpath)
        return Img(depth, keys, os.path.relpath(fullpath, outputdir))

    dump_table(annotated, key_columns, callback, basedir, outputdir, orientation='H')


CURRENT_DIRECTORY = os.path.abspath(os.curdir)
try:
    main()
except:
    os.chdir(CURRENT_DIRECTORY)
    raise
