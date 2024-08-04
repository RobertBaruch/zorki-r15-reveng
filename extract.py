from collections.abc import Sequence

from absl import app
from absl import flags

FLAGS = flags.FLAGS
flags.DEFINE_integer("first", None, "First sector to read")
flags.DEFINE_integer("last", None, "Last sector to read")
flags.DEFINE_integer("nsectors", None, "Number of sectors to read", short_name="n")
flags.DEFINE_string("infile", None, "Input file (dsk image)", short_name="i")
flags.DEFINE_string("outfile", None, "Output file", short_name="o")
flags.DEFINE_bool("skew", False, "Translate sectors to DOS ordering", short_name="s")

flags.mark_flag_as_required("first")
flags.mark_flag_as_required("infile")
flags.mark_flag_as_required("outfile")


def physical_sector(logical_sector: int) -> int:
    translate_table = [0x00, 0x0D, 0x0B, 0x09, 0x07, 0x05, 0x03, 0x01,
                       0x0E, 0x0C, 0x0A, 0x08, 0x06, 0x04, 0x02, 0x0F]

    track = logical_sector // 16
    sector = logical_sector % 16
    return track * 16 + translate_table[sector]


def main(argv: Sequence[str]) -> None:
    del argv
    if FLAGS.last is not None and FLAGS.nsectors is not None:
        print("You may specify either --last or --nsectors, but not both.")
        return
    if FLAGS.last is None and FLAGS.nsectors is None:
        print("You must specify either --last or --nsectors.")
        return

    first = FLAGS.first
    if FLAGS.nsectors is None:
        last = FLAGS.last
    else:
        last = FLAGS.first + FLAGS.nsectors - 1


    with (open(FLAGS.infile, mode="rb") as infile,
          open(FLAGS.outfile, mode="wb") as outfile):
        for sector in range(first, last+1):
            print(f"Read sector {sector}")
            if FLAGS.skew:
                sector = physical_sector(sector)
                print(f"  Translated to image sector {sector}")
            infile.seek(sector * 256)
            data = infile.read(256)
            outfile.write(data)


if __name__ == "__main__":
    app.run(main)
